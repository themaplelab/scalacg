package callgraph

import java.io.PrintStream

import scala.collection.mutable
import scala.reflect.io.AbstractFile
import scala.tools.nsc

import ca.uwaterloo.scalacg.util.Probe
import probe.CallGraph
import scalacg.probe.CallEdge
import scalacg.probe.CallSiteContext
import scalacg.probe.GXLWriter

trait CGUtils extends Probe {
  val global: nsc.Global // same as the other global
  import global._
  import global.definitions._

  val UNANNOT = "<unannotated>"

  def trees: List[Tree] // this is overridden by var trees in CallGraphPlugin

  case class CallSite(receiver: Tree, method: MethodSymbol, args: List[Tree],
    annotation: List[String], ancestors: List[Tree], pos: Position)

  var callSites = List[CallSite]()
  val callSitesInMethod = mutable.Map[Symbol, Set[CallSite]]()
  var classes = Set[Symbol]()
  var appClasses = Set[Symbol]()
  def callGraph: CallSite => Set[Symbol]

  // a set of the static targets found by the analysis
  var staticTargets = Set[Symbol]()

  def annotationFilter: PartialFunction[Tree, String]

  abstract class TraverseWithAncestors {
    def visit(node: Tree, ancestors: List[Tree])
    def traverse(tree: Tree, ancestors: List[Tree]): Unit = {
      visit(tree, ancestors)
      tree.children.foreach { child =>
        traverse(child, tree :: ancestors)
      }
    }
    def apply(root: Tree) = traverse(root, List())
  }
  def addCallSite(callSite: CallSite) = {
    callSites = callSite :: callSites
    val inMethod = enclosingMethod(callSite)
    callSitesInMethod(inMethod) =
      callSitesInMethod.getOrElse(inMethod, Set()) + callSite
  }

  /**
   * Find the enclosing method of a call site. For call sites in methods, that's obvious. For call sites
   * that appear in class definition, the primary constructor of the defining class is the enclosing method.
   */
  def enclosingMethod(callSite: CallSite) = {
    // Find a method that contains this call site.
    val defdef = callSite.ancestors.find({
      node => node.isInstanceOf[DefDef]
    })

    // If none found, then it should be in a class definition, and the enclosing method is the primary constructor
    if (defdef.isEmpty) {
      val classdef = callSite.ancestors.find({
        node => node.isInstanceOf[ClassDef]
      })
      classdef.get.symbol.primaryConstructor
    } else {
      defdef.get.symbol
    }
  }

  // look for an annotation on the receiver
  def findReceiverAnnotations(receiver: Tree): (List[String], Tree) = {
    val (annotation, plainReceiver) =
      receiver match {
        case Block(annotations, plainReceiver) =>
          val annot = annotations.collect(annotationFilter)
          (annot, plainReceiver)
        case _ => (List(), receiver)
      }
    (annotation, plainReceiver)
  }
  def initialize = {
    // find the set of instantiated classes in the whole program
    classes = trees.flatMap { tree =>
      tree.collect {
        case cd: ClassDef if cd.symbol.isModuleClass => cd.symbol // isModuleClass -> an object, it gets auto instantiated
        case nw: New => nw.tpt.symbol // the set of all allocation sites
      }
    }.toSet

    // find call sites
    trees.foreach { tree =>
      findCallSites(tree, List())
    }

    // TODO
    //        println(callSites.filter(_.method.name.decode.equals("lineNr_=")).map(_.ancestors.find({
    //          node => node.isInstanceOf[DefDef] || node.isInstanceOf[ClassDef]
    //        }).get.symbol))
  }

  def normalizeMultipleParameters(tree: Apply): Apply = tree.fun match {
    case a: Apply => normalizeMultipleParameters(a)
    case _ => tree
  }

  /** Returns the receiver of an apply or unapply. Some don't have a receiver. */
  def getReceiver(tree: Tree): Option[Tree] = tree match {
    case a: Apply => getReceiver(a.fun)
    case s: Select => Some(s.qualifier)
    case t: TypeApply => getReceiver(t.fun)
    case _: Ident => None
    case _ => assert(false, "getReceiver on unexpected tree " + tree + " of type " + tree.getClass); null
  }

  def findCallSites(tree: Tree, ancestors: List[Tree]) {
    def processChildren() {
      tree.children.foreach(findCallSites(_, tree :: ancestors))
    }

    def isMethod(symbol: Symbol) =
      symbol.isInstanceOf[MethodSymbol] &&
        symbol.name != nme.OUTER_SYNTH && // this is a fake method that never exists; such calls are
        // later replaced by calls to a synthetic accessor method
        !symbol.isLabel // gotos are implemented with a method-call-like syntax, but they do not call
    // actual methods, only labels

    tree match {
      // trees that invoke methods
      case apply: Apply =>
        val a = normalizeMultipleParameters(apply)
        val callee = a.fun
        val args = a.args
        val receiver = getReceiver(a)
        if (isMethod(callee.symbol)) {
          val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
          addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, args, annotation, ancestors, tree.pos))
        }
        args.foreach(findCallSites(_, tree :: ancestors))
        callee.children.foreach(findCallSites(_, tree :: ancestors))

      case _: Select | _: Ident =>
        if (isMethod(tree.symbol)) {
          val receiver = getReceiver(tree)
          val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
          addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, List(), annotation, ancestors, tree.pos))
        }
        processChildren

      case _ =>
        processChildren
    }
  }

  def findTargetAnnotation(symbol: Symbol): String =
    findAnnotationTargets(symbol, "callgraph.annotation.target", needProbe = true).head

  def findAnnotationTargets(symbol: Symbol, annotationName: String, needProbe: Boolean): List[String] = {
    val targetAnnotationType = rootMirror.getRequiredClass(annotationName).tpe
    val targets = symbol.annotations.collect {
      case AnnotationInfo(tpe, args, _) if tpe == targetAnnotationType =>
        args.map((arg) =>
          arg match {
            case Literal(Constant(string: String)) => string
          })
    }
    assert(targets.size <= 1)
    targets.headOption.getOrElse(if (needProbe)
      List(UNANNOT + " " + probeMethod(symbol))
    else Nil)
  }

  var concretization = Map[Symbol, Set[Type]]()
  def addTypeConcretizations(classes: Set[Symbol]) = {
    // find all definitions of abstract type members ("type aliases")
    for {
      cls <- classes
      sym <- cls.tpe.members // concrete type
      superClass <- cls.baseClasses
      absSym <- superClass.tpe.decls // abstract type
      if absSym.isAbstractType
      if absSym.name == sym.name
    } {
      concretization +=
        (absSym -> (concretization.getOrElse(absSym, Set()) + sym.tpe.dealias))
    }

    // find all instantiations of generic type parameters (generics behave the same way 
    for {
      cls <- classes
    } {
      cls.info match {
        // class declaration and has a set of parents
        case ClassInfoType(parents, _, _) =>
          for { parent <- parents } {
            val args = parent.typeArguments
            val cstr = parent.typeConstructor
            val params = cstr.typeParams
            for {
              (arg, param) <- (args zip params)
            } {
              concretization +=
                (param -> (concretization.getOrElse(param, Set() + arg)))
            }
          }
        case _ =>
        // TODO: are we missing any cases?
        // one case missing is new List[Int]
      }
    }

    // transitively follow abstract type concretizations
    var oldConcretization = concretization
    do {
      oldConcretization = concretization
      for {
        (absSym, tpes) <- concretization
        tpe <- tpes
      } {
        tpe match {
          case TypeRef(_, sym, _) =>
            concretization +=
              (absSym -> (concretization(absSym) ++ concretization.getOrElse(sym, Set())))
          case _ =>
        }
      }
    } while (oldConcretization != concretization)
  }
  def expand(t: Type): Set[Type] = {
    val sym = t.typeSymbol
    if (sym.isAbstractType) {
      concretization.getOrElse(sym, Set())
    } else {
      Set(t)
    }
  }

  /**
   * The main method lookup for Scala.
   */
  def lookup(receiverType: Type, staticTarget: MethodSymbol, consideredClasses: Set[Symbol]): Set[Symbol] = {
    // If the target method is a constructor, no need to do the lookup.
    if (staticTarget.isConstructor) {
      Set(staticTarget)
    } else {
      var targets = List[Symbol]()

      def instantiateTypeParams(actual: Type, declared: Type): Type = {
        val tparams = declared.typeArgs
        val args = tparams map
          { _.asSeenFrom(ThisType(actual.typeSymbol), declared.typeSymbol) }
        declared.instantiateTypeParams(tparams map { _.typeSymbol }, args)
      }

      for {
        cls <- consideredClasses
        val tpe = cls.tpe
        expandedType <- expand(instantiateTypeParams(tpe, receiverType.widen))
        if tpe <:< expandedType
        val target = tpe.member(staticTarget.name)
        if !target.isDeferred
      } {
        target match {
          case NoSymbol =>
            // TODO: can this ever happen? let's put in an assertion and see...
            assert(false, "tpe is " + tpe)

          case _ =>
            // Disambiguate overloaded methods based on the types of the args
            if (target.isOverloaded) {
              targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)) ::: targets
            } else {
              targets = target :: targets
            }
        }
      }
      // If the target method is a Java method, or a Scala library method, the lookup won't yield anything. Just return
      // the static target.
      // TODO ignore this for now for the sake of making some progress on the experiments!
      //      if (targets.isEmpty) {
      //        targets = List[Symbol](staticTarget)
      //        staticTargets += staticTarget
      //        println(bytecodeSignature(staticTarget))
      //      }

      targets.toSet
    }
  }

  def printAnnotatedCallsites() {
    printTargets()
    printInvocations()
  }

  private def printTargets() {
    for {
      callSite <- callSites
      if !callSite.annotation.isEmpty
    } {
      println(callSite.method + " " + callSite.annotation)

      val resolved = callGraph(callSite).map(findTargetAnnotation)
      val expected = callSite.annotation.toSet
      printCallGraph(resolved, isResolved = true)
      printCallGraph(expected, isResolved = false)
      assert(callSite.annotation.isEmpty || (resolved == expected), expected.toSeq.sorted.mkString(", "))
    }
  }

  def printCallGraph(methodNames: Set[String], isResolved: Boolean) {
    val resolvedExpected = if (isResolved) "Resolved: " else "Expected: "
    println(resolvedExpected + methodNames.toSeq.sorted.mkString(", "))
  }

  private def printInvocations() {
    for {
      method <- reachableMethods
      if !method.annotations.isEmpty
    } {
      val expected: Set[String] =
        findAnnotationTargets(method, "callgraph.annotation.invocations", needProbe = false).toSet
      if (expected.isEmpty)
        return
      val resolved: Set[String] =
        callSitesInMethod(method).flatMap((cs: CallSite) =>
          callGraph(cs).map((s: Symbol) =>
            cs.pos.line + ": " + findTargetAnnotation(s)))

      //        callSitesInMethod(method).flatMap(callGraph).map(findTargetAnnotation) // add line numbers
      printCallGraph(resolved, isResolved = true)
      printCallGraph(expected, isResolved = false)
      assert(expected.subsetOf(resolved), expected.toSeq.sorted.mkString(", "))
    }
  }

  def transitiveClosure[T](initial: Set[T], transition: T => Set[T]): Set[T] = {
    val seen = mutable.Set[T]() ++ initial
    val queue = mutable.Queue[T]() ++ initial
    while (!queue.isEmpty) {
      val item = queue.dequeue
      val image = transition(item) -- seen
      queue ++= image
      seen ++= image
    }
    Set() ++ seen
  }

  /**
   * These are methods that should be reachable due to some library call back.
   * For now, we only consider methods that override library methods as callbacks.
   */
  def callbacks = {
    var callbacks = Set[Symbol]()
    for {
      cls <- appClasses
      member <- cls.tpe.decls // loop over the declared members, "members" returns defined AND inherited members
      if member.isMethod && !member.isDeferred && member.allOverriddenSymbols.nonEmpty
      val libraryOverriddenSymbols = member.allOverriddenSymbols.filterNot(appClasses contains _.owner)
      overridden <- libraryOverriddenSymbols
    } {
      callbacks += member
    }
    callbacks
  }

  def entryPoints = mainMethods

  // return all main methods that are inherited into some object
  def mainMethods = {
    val mainName = stringToTermName("main")

    val mainMethods = classes.filter(_.isModuleClass). // filter classes that are objects
      collect { case cs: ClassSymbol => cs.tpe.member(mainName) }. // collect main methods
      filter(_.isMethod). // consider only methods, not fields or other members
      filter(!_.isDeferred). // filter out abstract methods
      filter(_.typeSignature.toString.equals("(args: Array[String])Unit")) // filter out methods accidentally named "main"
    // global.definitions.StringArray

    Set() ++ mainMethods
  }

  lazy val reachableMethods = transitiveClosure(entryPoints ++ callbacks, { source: Symbol =>
    for {
      callSite <- callSitesInMethod.getOrElse(source, Set())
      target <- callGraph(callSite)
    } yield target: Symbol
  })

  val methodToId: collection.Map[Symbol, Int]
  def printCallGraph(out: java.io.PrintStream) = {
    for {
      source <- reachableMethods
      val sourceId = methodToId.getOrElse(source, 0)
      callSite <- callSitesInMethod.getOrElse(source, Set())
      target <- callGraph(callSite)
      val targetId = methodToId.getOrElse(target, 0)
    } out.println(sourceId + " " + targetId)
  }
  def printTextualCallGraph(out: java.io.PrintStream) = {
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set())
      target <- callGraph(callSite)
    } out.println(probeMethod(source) + " ==> " + probeMethod(target))
  }

  def formatPosition(pos: Position, method: Symbol) = {
    (if (pos.isDefined) relativize(pos.source.file) + " ::: " + pos.line
    else "unknown ::: 0") + " ::: " + probeMethod(method)
  }

  def formatPosition(pos: Position) = {
    if (pos.isDefined) relativize(pos.source.file) + " ::: " + pos.line
    else "unknown ::: 0"
  }

  def printEclipseCallGraph(out: java.io.PrintStream) = {
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set())
      target <- callGraph(callSite)
    } {
      out.println(
        formatPosition(callSite.pos, source) + " ==> " +
          formatPosition(target.pos, target))
    }
  }

  def printReachableMethods(out: java.io.PrintStream) = {
    for (method <- reachableMethods)
      out.println(methodToId.getOrElse(method, 0) + " ::: " + formatPosition(method.pos, method))
  }

  /**
   * Print the mapping of all annotated methods to their source level signature.
   */
  def printMethods(out: java.io.PrintStream) = {
    for (method <- methodToId.keys) {
      out.println(methodToId.getOrElse(method, 0) + " ===> " + probeMethod(method))
    }
  }

  /**
   * Print the mapping of the annotated methods to their source level effective owner.
   */
  def printMethodsOnwers(out: java.io.PrintStream) = {
    for (method <- methodToId.keys) {
      out.println(methodToId.getOrElse(method, 0) + " ===> " + effectiveOwnerName(method))
    }
  }

  /**
   * Return a Soot-like method signature.
   */
  def methodSignature(methodSymbol: Symbol): String = {
    "<" + methodSymbol.fullName + methodSymbol.signatureString + ">"
  }

  /**
   * Return a probe call graph in GXL format.
   */
  def printProbeCallGraph(out: java.io.PrintStream) = {
    val probeCallGraph = new CallGraph
    val entryPointsOut = new PrintStream("entrypoints.txt")

    // Get the entry points
    for {
      entry <- entryPoints
    } {
      probeCallGraph.entryPoints.add(probeMethod(entry))
      entryPointsOut.println(methodToId.getOrElse(entry, 0) + " ===> " + probeMethod(entry))
    }

    // Get the edges
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set())
      target <- callGraph(callSite)
      val sourceFile = relativize(callSite.pos.source.file)
      val line = callSite.pos.line.toString
    } probeCallGraph.edges.add(new CallEdge(probeMethod(source), probeMethod(target), new CallSiteContext(sourceFile + " : line " + line)))

    // Write GXL file
    new GXLWriter().write(probeCallGraph, out)
  }

  /**
   * Get the relative path for an absolute source file path.
   */
  def relativize(file: AbstractFile): String = {
    file.toString.replaceFirst(".+/build_src/[^/]+/", "")
  }

  /**
   * Get the relative path for an absolute source file path.
   */
  def relativize(absolute: String): String = {
    absolute.replaceFirst(".+/build_src/[^/]+/", "")
  }

  /**
   * Get the bytecode descriptor for the given symbol
   */
  def bytecodeSignature(methodSymbol: Symbol): String = {
    var bf = ""

    //    bf += methodSymbol.effectiveOwner.javaClassName
    //    bf += ": " + methodSymbol.simpleName.encode
    //    bf += " ("
    for {
      param <- methodSymbol.tpe.params
    } {
      bf += signature(param.tpe)
    }
    //    bf += ")"
    bf
  }

  /**
   * Get the bytecode signature of a type
   */
  def signature(tpe: Type): String = {
    tpe.erasure match {
      case BooleanTpe => "Z"
      case ByteTpe => "B"
      case CharTpe => "C"
      case DoubleTpe => "D"
      case FloatTpe => "F"
      case IntTpe => "I"
      case LongTpe => "L"
      case ShortTpe => "S"
      case TypeRef(_, ArrayClass, arg :: Nil) => ("[") + signature(arg)
      case UnitTpe => "V"
      case _ if tpe.erasure <:< AnyRefTpe => "L" + tpe.erasure.typeSymbol.javaBinaryName + ";"
      case _ => assert(false, "unknown type: " + tpe.erasure); null
    }
  }
}
package callgraph

import java.io.PrintStream

import scala.collection.mutable
import scala.reflect.io.AbstractFile
import scala.tools.nsc

import ca.uwaterloo.scalacg.util.Annotations
import ca.uwaterloo.scalacg.util.Probe
import probe.CallGraph
import scalacg.probe.CallEdge
import scalacg.probe.CallSiteContext
import scalacg.probe.GXLWriter

trait CGUtils extends Probe with Annotations {
  val global: nsc.Global // same as the other global
  import global._
  import global.definitions._

  val UNANNOT = "<unannotated>"

  def trees: List[Tree] // this is overridden by var trees in CallGraphPlugin

  def instantiatedClasses: Set[Type]
  def reachableCode: Set[Symbol]
  def callbacks: Set[Symbol]
  def appClasses: Set[Type] // application classes fed to the compiler (i.e., not including Scala/Java libraries)
  def callGraph: CallSite => Set[Symbol]

  case class CallSite(receiver: Tree, staticTarget: MethodSymbol, args: List[Tree], annotation: List[String],
    ancestors: List[Tree], pos: Position, enclMethod: Symbol)

  /**
   * Find the enclosing method from the given list of ancestors. For call sites in methods, that's obvious. For call sites
   * that appear in class definition, the primary constructor of the defining class is the enclosing method.
   */
  def enclMethod(ancestors: List[Tree]) = {
    ancestors.collectFirst {
      case cd: ClassDef => cd.symbol.primaryConstructor
      case dd: DefDef => dd.symbol
    }.get
  }

  var callSites = List[CallSite]()
  val callSitesInMethod = mutable.Map[Symbol, Set[CallSite]]()
  var classes = Set[Type]()

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
    callSitesInMethod(callSite.enclMethod) =
      callSitesInMethod.getOrElse(callSite.enclMethod, Set()) + callSite
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

  def initialize() {
    // find the set of instantiated classes in the whole program
    classes = trees.flatMap { tree =>
      tree.collect {
        case cd: ClassDef if cd.symbol.isModuleOrModuleClass => cd.symbol.tpe // isModuleClass -> an object, it gets auto instantiated
        case nw: New => nw.tpt.tpe // the set of all allocation sites
      }
    }.toSet

    // find call sites
    trees.foreach { tree =>
      findCallSites(tree, List())
    }

    for (callSite <- callSites) println(signature(callSite.enclMethod) + " ===> " + signature(callSite.staticTarget))
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
      // Add the calls from primary constructors of classes to mixin constructors (see AbstractTypes13)
      case cls: ClassDef =>
        for {
          mixin <- cls.symbol.mixinClasses
          val caller = cls.symbol.primaryConstructor
          val callee = mixin.primaryConstructor
          if caller != NoSymbol && callee != NoSymbol
          val receiver = This(caller.thisSym)
        } {
          addCallSite(CallSite(receiver, callee.asMethod, List[Tree](), List[String](), ancestors, tree.pos, caller))
        }
        processChildren
      // trees that invoke methods
      case apply: Apply =>
        val a = normalizeMultipleParameters(apply)
        val callee = a.fun
        val args = a.args
        val receiver = getReceiver(a)
        if (isMethod(callee.symbol)) {
          val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
          val _enclMethod = enclMethod(ancestors)
          addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, args, annotation, ancestors, tree.pos, _enclMethod))
        }
        args.foreach(findCallSites(_, tree :: ancestors))
        callee.children.foreach(findCallSites(_, tree :: ancestors))
      case _: Select | _: Ident =>
        if (isMethod(tree.symbol)) {
          val receiver = getReceiver(tree)
          val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
          val _enclMethod = enclMethod(ancestors)
          addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, List(), annotation, ancestors, tree.pos, _enclMethod))
        }
        processChildren()

      case _ =>
        processChildren()
    }
  }

  def findTargetAnnotation(symbol: Symbol): String =
    findAnnotationTargets(symbol, targetAnnotation, needProbe = true).head

  def findAnnotationTargets(symbol: Symbol, annotation: ClassSymbol, needProbe: Boolean): List[String] = {
    val targetAnnotationType = annotation.tpe
    val targets = symbol.annotations.collect {
      case AnnotationInfo(tpe, args, _) if tpe == targetAnnotationType =>
        args.map((arg) =>
          arg match {
            case Literal(Constant(string: String)) => string
          })

    }
    targets.headOption.getOrElse(if (needProbe)
      List(UNANNOT + " " + probeMethod(symbol))
    else Nil)
  }

  var concretization = Map[Symbol, Set[Type]]()
  def addTypeConcretizations(classes: Set[Type]) = {
    // find all definitions of abstract type members ("type aliases")
    for {
      tpe <- classes
      sym <- tpe.members // concrete type
      superClass <- tpe.baseClasses
      absSym <- superClass.tpe.decls // abstract type
      if absSym.isAbstractType
      if absSym.name == sym.name
    } {
      concretization +=
        (absSym -> (concretization.getOrElse(absSym, Set()) + sym.tpe.dealias))
    }

    // find all instantiations of generic type parameters (generics behave the same way)
    for {
      tpe <- classes
      val cls = tpe.typeSymbol
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
        case PolyType(typeParams, _) =>
          // handles the case: new List[Int]
          for {
            (arg, param) <- (tpe.typeArguments zip typeParams)
          } {
            concretization +=
              (param -> (concretization.getOrElse(param, Set() + arg)))
          }
        case _ =>
        // TODO: are we missing any cases?
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

  /**
   * Is this symbol in an application class?
   */
  def isApplication(symbol: Symbol) = {
    appClasses contains symbol.enclClass.tpe
  }

  /**
   * Is this symbol in a library class?
   */
  def isLibrary(symbol: Symbol) = {
    !isApplication(symbol)
  }

  def expand(t: Type): Set[Type] = {
    val sym = t.typeSymbol
    if (sym.isAbstractType) {
      concretization.getOrElse(sym, Set())
    } else {
      Set(t)
    }
  }

  def instantiateTypeParams(actual: Type, declared: Type): Type = {
    val tparams = declared.typeArgs

    // Using `actual` rather than `ThisType(actual.typeSymbol)`, the latter causes loss of generic type information
    // see (Generics4)
    val args = tparams map { _.asSeenFrom(actual, declared.typeSymbol) }

    declared.instantiateTypeParams(tparams map { _.typeSymbol }, args)
  }

  def superLookup(receiverType: Type, staticTarget: MethodSymbol, consideredClasses: Set[Type]): Set[Symbol] =
    lookup(receiverType, staticTarget, consideredClasses, lookForSuperClasses = true,
      getSuperName = ((name: String) => if (name.startsWith("super$")) name.substring("super$".length) else name))

  /**
   * The main method lookup for Scala.
   */
  def lookup(receiverType: Type, staticTarget: MethodSymbol, consideredClasses: Set[Type],
    // default parameters, used only for super method lookup 
    lookForSuperClasses: Boolean = false, getSuperName: (String => String) = (n: String) => n): Set[Symbol] = {
    // If the target method is a constructor, no need to do the lookup.
    if (staticTarget.isConstructor) {
      Set(staticTarget)
    } else {
      var targets = List[Symbol]()
      for {
        tpe <- consideredClasses
        expandedType <- expand(instantiateTypeParams(tpe, receiverType.widen))
        asf = expandedType.asSeenFrom(tpe, expandedType.typeSymbol)
        if tpe <:< asf || lookForSuperClasses
        target = if (lookForSuperClasses)
          tpe.member(staticTarget.name.newName(getSuperName(staticTarget.nameString))) // todo: bad bad bad
        else tpe.member(staticTarget.name)
        if !target.isDeferred
      } {
        target match {
          case NoSymbol =>
            // TODO: can this ever happen? let's put in an assertion and see...
            assert(assertion = false, message = "tpe is " + tpe)

          case _ =>
            // Disambiguate overloaded methods based on the types of the args
            if (target.isOverloaded) {
              targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)) ::: targets
            } else {
              targets = target :: targets
            }

            // If we resolve to a library method, remove it and instead put its topmostLibraryOverridenMethod.
            val libraryTargets = targets filter isLibrary
            val topmosts = (libraryTargets map topmostLibraryOverriddenMethod) filter (_ != NoSymbol)
            targets = targets filter isApplication
            targets = topmosts ::: targets
        }
      }

      /*
       * If targets is empty, then check if the staticTarget is in the library, return it. That means that the method may 
       * only resolve to a method in the library, since we didn't resolve it to any method in the application. On the
       * other hand, if the staticTarget is in the application, then this only means that this method should resolve to 
       * something in the library. For that, we need to get the topmost overridden library method.
       */
      if (targets.isEmpty) {
        if (isLibrary(staticTarget)) {
          targets = List[Symbol](staticTarget)
        } else {
          val topmost = topmostLibraryOverriddenMethod(staticTarget)
          if (topmost != NoSymbol) targets = List[Symbol](topmost)
        }
      }

      targets.toSet
    }
  }

  /**
   * Get all the library methods overridden by this method. The methods are sorted according to the reverse
   * linearization.
   */
  def libraryOverriddenMethods(symbol: Symbol) = {
    var result = List[Symbol]()
    if (symbol.isMethod && symbol.allOverriddenSymbols.nonEmpty) {
      result = symbol.allOverriddenSymbols.filter(sym => isLibrary(sym))
    }
    result
  }

  /**
   * Get the topmost library method overridden by the this method. If the method is in the library and doesn't
   * override any method, then it is its own topmostLibraryOverriddenMethod
   */
  def topmostLibraryOverriddenMethod(symbol: Symbol) = {
    val methods = libraryOverriddenMethods(symbol)
    if (methods.nonEmpty) methods.reverse.head
    else if (isLibrary(symbol)) symbol
    else NoSymbol
  }

  def printAnnotatedCallsites() {
    printTargets()
    printInvocations()
  }

  private def printTargets() {
    for {
      callSite <- callSites
      if reachableCode contains callSite.enclMethod
      expected = callSite.annotation.toSet
      if expected.nonEmpty
    } {
      println(callSite.staticTarget + " " + callSite.annotation)

      val resolved = callGraph(callSite).map(findTargetAnnotation)
      printCallGraph(resolved, isResolved = true)
      printCallGraph(expected, isResolved = false)
      assert((expected == Set(NONE) && resolved.isEmpty) || (resolved == expected), expected.toSeq.sorted.mkString(", "))
    }
  }

  def printCallGraph(methodNames: Set[String], isResolved: Boolean) {
    val resolvedExpected = if (isResolved) "Resolved: " else "Expected: "
    println(resolvedExpected + methodNames.toSeq.sorted.mkString(", "))
  }

  private def printInvocations() {
    val symbols: Set[Symbol] = reachableMethods ++ instantiatedClasses.map(_.typeSymbol)
    for {
      symbol <- symbols
      noInvocations = hasNoInvocationsAnnotation(symbol)
      if noInvocations || hasInvocationsAnnotation(symbol)
      expected = findAnnotationTargets(symbol, invocationsAnnotation, needProbe = false).toSet
      hasExpected = expected.nonEmpty
      if hasExpected || noInvocations
    } {
      assert(hasExpected != noInvocations, "@invocations should not be combined with @noInvocations for the same symbol")
      val methodOrConstructor: Symbol = if (symbol.isMethod) symbol else symbol.primaryConstructor
      val callSitesInMethodOrConstructor = if (callSitesInMethod.contains(methodOrConstructor))
        callSitesInMethod(methodOrConstructor)
      else Set()
      val resolved: Set[String] =
        callSitesInMethodOrConstructor.flatMap((cs: CallSite) =>
          callGraph(cs).map(cs.pos.line + ": " + findTargetAnnotation(_)))
      printCallGraph(resolved, isResolved = true)
      printCallGraph(expected, isResolved = false)
      assert(if (noInvocations) resolved.isEmpty else expected.subsetOf(resolved),
        (expected &~ resolved).toSeq.sorted.mkString(", "))
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

  lazy val entryPoints = mainMethods

  // return all main methods that are inherited into some object
  def mainMethods = {
    val mainName = stringToTermName("main")

    val mainMethods = classes.filter(_.typeSymbol.isModuleOrModuleClass). // filter classes that are objects
      collect { case cs: ModuleTypeRef => cs.member(mainName) }. // collect main methods
      filter(m => m.isMethod && // consider only methods, not fields or other members
        !m.isDeferred && // filter out abstract methods
        m.typeSignature.toString.equals("(args: Array[String])Unit")) // filter out methods accidentally named "main"

    // global.definitions.StringArray

    Set() ++ mainMethods
  }

  lazy val reachableMethods = transitiveClosure(entryPoints ++ callbacks, { source: Symbol =>
    for {
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
      target <- callGraph(callSite)
    } yield target: Symbol
  })

  val methodToId: collection.Map[Symbol, Int]
  def printCallGraph(out: java.io.PrintStream) = {
    for {
      source <- reachableMethods
      val sourceId = methodToId.getOrElse(source, 0)
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
      target <- callGraph(callSite)
      val targetId = methodToId.getOrElse(target, 0)
    } out.println(sourceId + " " + targetId)
  }
  def printTextualCallGraph(out: java.io.PrintStream) = {
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
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
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
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
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
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
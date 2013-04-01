package callgraph

import scala.tools.nsc
import scala.collection.mutable
import scala.collection
import probe.CallGraph
import probe.ProbeMethod
import probe.ObjectManager
import probe.CallEdge
import probe.GXLWriter

trait CGUtils {
  val global: nsc.Global
  import global._

  val UNANNOT = "<unannotated>"

  def trees: List[Tree]

  case class CallSite(receiver: Tree, method: MethodSymbol, args: List[Tree], annotation: List[String], ancestors: List[Tree])

  var callSites = List[CallSite]()
  val callSitesInMethod = mutable.Map[Symbol, Set[CallSite]]()
  var classes = Set[ClassSymbol]()
  def callGraph: CallSite => Set[MethodSymbol]

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
    val enclosingMethod = callSite.ancestors.find({
      node => node.isInstanceOf[DefDef] || node.isInstanceOf[ClassDef]
    }).get.symbol
    callSitesInMethod(enclosingMethod) =
      callSitesInMethod.getOrElse(enclosingMethod, Set()) + callSite
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
    // find call sites
    trees.foreach { tree =>
      findCallSites(tree, List())
    }

    // find classes
    classes = trees.flatMap { tree =>
      tree.collect { case cd: ClassDef => cd.symbol.asClass }
    }.toSet
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
          addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, args, annotation, ancestors))
        }
        args.foreach(findCallSites(_, tree :: ancestors))
        callee.children.foreach(findCallSites(_, tree :: ancestors))

      case _: Select | _: Ident =>
        if (isMethod(tree.symbol)) {
          val receiver = getReceiver(tree)
          val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
          addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, List(), annotation, ancestors))
        }
        processChildren

      case _ =>
        processChildren
    }
  }

  def findTargetAnnotation(symbol: Symbol) = {
    val targetAnnotationType =
      rootMirror.getRequiredClass("tests.target").tpe
    val targets = symbol.annotations.collect {
      case AnnotationInfo(tpe, Literal(Constant(string: String)) :: _, _) if tpe == targetAnnotationType => string
    }
    assert(targets.size <= 1)
    targets.headOption.getOrElse(UNANNOT)
  }

  var concretization = Map[Symbol, Set[Type]]()
  def addTypeConcretizations(classes: Set[ClassSymbol]) = {
    // find all definitions of abstract type members ("type aliases")
    for {
      cls <- classes
      sym <- cls.tpe.members
      if sym.isAliasType
      superClass <- cls.baseClasses
      absSym <- superClass.tpe.decls
      if absSym.isAbstractType
      if absSym.name == sym.name
    } {
      concretization +=
        (absSym -> (concretization.getOrElse(absSym, Set()) + sym.tpe))
    }

    // find all instantiations of generic type parameters
    for {
      cls <- classes
    } {
      cls.info match {
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
    } else Set(t)
  }

  def lookup(receiverType: Type, staticTarget: MethodSymbol, consideredClasses: Set[ClassSymbol]): Set[MethodSymbol] = {
    if (staticTarget.isConstructor)
      Set(staticTarget)
    else {
      var targets = List[MethodSymbol]()
      for {
        cls <- consideredClasses
        val tpe = cls.tpe
        expandedType <- expand(receiverType.widen)
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
              targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)).map(_.asMethod) ::: targets
            } else {
              targets = target.asMethod :: targets
            }
        }
      }
      targets.toSet
    }
  }

  def printAnnotatedCallsites = {
    for {
      callSite <- callSites
      if !callSite.annotation.isEmpty
    } {
      println(callSite.method + " " + callSite.annotation)

      val resolved = callGraph(callSite).map(findTargetAnnotation)
      val expected = callSite.annotation.toSet
      println("Resolved: " + resolved.toSeq.sorted.mkString(", "))
      println("Expected: " + expected.toSeq.sorted.mkString(", "))
      assert(callSite.annotation.isEmpty || (resolved == expected), expected.toSeq.sorted.mkString(", "))
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

  def entryPoints = mainMethods

  // just takes the first main method that it finds
  def mainMethods = {
    val mainName = stringToTermName("main")

    // all classes encountered in the source files
    val classes = trees.toStream.flatMap { tree =>
      tree.collect { case cd: ClassDef => cd.symbol.asClass }
    }

    // all main methods encountered in those classes
    val mainMethods = classes.collect {
      case cs: ClassSymbol => cs.tpe.member(mainName)
    }.filter(_ != NoSymbol).filter(!_.isDeferred).filter(_.isMethod)
    Set() ++ mainMethods
  }

  lazy val reachableMethods = transitiveClosure(entryPoints, { source: Symbol =>
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
    } out.println(printableName(source) + " ==> " + printableName(target))
  }
  def printableName(method: Symbol) =
    method.fullName + method.signatureString
  def printReachableMethods(out: java.io.PrintStream) = {
    for (method <- reachableMethods)
      out.println(methodToId.getOrElse(method, 0) + " " + printableName(method))
  }

  /**
   * Return a Soot-like method signature.
   */
  def methodSignature(methodSymbol: MethodSymbol) = {
    "<" + methodSymbol.fullName.replace("." + methodSymbol.simpleName, "") + ": " + methodSymbol.simpleName + " " + methodSymbol.signatureString + ">"
  }

  /**
   * Return a probe call graph in GXL format.
   */
  def printProbeCallGraph(out: java.io.PrintStream) = {
    val probeCallGraph = new CallGraph

    // Get the entry points
    for (entryPoint <- entryPoints) {
      probeCallGraph.entryPoints.add(probeMethod(entryPoint))
    }

    for {
      source <- reachableMethods
      val sourceId = methodToId.getOrElse(source, 0)
      callSite <- callSitesInMethod.getOrElse(source, Set())
      target <- callGraph(callSite)
      val targetId = methodToId.getOrElse(target, 0)
    } {
      probeCallGraph.edges.add(new CallEdge(probeMethod(source), probeMethod(target)))
    }

    // Write GXL file
    new GXLWriter().write(probeCallGraph, out)
  }

  /**
   * Get a probe method for the given symbol
   */
  def probeMethod(methodSymbol: Symbol): ProbeMethod = {
    val probeClass = ObjectManager.v().getClass(methodSymbol.owner.signatureString)
    val probeMethod = ObjectManager.v().getMethod(probeClass, methodSymbol.simpleName.longString, methodSymbol.signatureString)
    probeMethod
  }
}

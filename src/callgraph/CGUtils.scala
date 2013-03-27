package callgraph

import scala.tools.nsc
import scala.collection.mutable
import scala.collection

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
  def initialize = {
    // find call sites
    trees.foreach { tree =>
      (new TraverseWithAncestors {
        def visit(node: Tree, ancestors: List[Tree]) = {
          node match {
            case Apply(Select(receiver, methodName), args) =>
              // look for an annotation on the receiver
              val (annotation, plainReceiver) =
                receiver match {
                  case Block(annotations, plainReceiver) =>
                    val annot = annotations.collect(annotationFilter)
                    (annot, plainReceiver)
                  case _ => (List(), receiver)
                }
              addCallSite(CallSite(plainReceiver, node.symbol.asMethod, args, annotation, ancestors))
            case _ =>
          }
        }
      })(tree)
    }

    // find classes
    classes = trees.flatMap { tree =>
      tree.collect { case cd: ClassDef => cd.symbol.asClass }
    }.toSet
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
    if (staticTarget.isConstructor) Set(staticTarget) else {
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

  def entryPoints = Set(mainMethod)

  // just takes the first main method that it finds
  def mainMethod = {
    val mainName = stringToTermName("main")
    
    // all classes encountered in the source files
    val classes = trees.toStream.flatMap { tree =>
      tree.collect { case cd: ClassDef => cd.symbol.asClass }
    }
    
    // all main methods encountered in those classes
    val mainMethods = classes.collect{
      case cs: ClassSymbol => cs.tpe.member(mainName)
    }.filter(_ != NoSymbol).filter(!_.isDeferred).filter(_.isMethod)
    
    // the first main method
    mainMethods.head
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
  def printReachableMethods(out: java.io.PrintStream) = {
    for (method <- reachableMethods)
      out.println(methodToId.getOrElse(method, 0) + " " + method.fullName)
  }
}

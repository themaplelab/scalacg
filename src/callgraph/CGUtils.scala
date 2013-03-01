package callgraph

import scala.tools.nsc

trait CGUtils {
  val global: nsc.Global
  import global._

  val UNANNOT = "<unannotated>"

  def trees: List[Tree]

  case class CallSite(receiver: Tree, method: MethodSymbol, args: List[Tree], annotation: List[String], ancestors: List[Tree])

  var callSites = List[CallSite]()
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
              callSites = CallSite(plainReceiver, node.symbol.asMethod, args, annotation, ancestors) :: callSites
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

  def lookup(receiverType: Type, staticTarget: MethodSymbol, consideredClasses: Set[ClassSymbol]): Set[MethodSymbol] = {
    var targets = List[MethodSymbol]()
    for {
      cls <- consideredClasses
      val tpe = cls.tpe
      if tpe <:< receiverType
      val target = tpe.member(staticTarget.name)
      if !target.isDeferred
    } {
      target match {
        case NoSymbol =>
          // TODO: can this ever happen? let's put in an assertion and see...
          assert(false)

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
}

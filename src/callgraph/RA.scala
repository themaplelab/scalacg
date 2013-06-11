package callgraph

import scala.tools.nsc

trait RA {
  this: CGUtils =>
  val global: nsc.Global

  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  def buildCallGraph() {
    for (callSite <- callSites) {
      val targets = nameLookup(callSite.method.name, classes)
      callGraph += (callSite -> targets)
    }
  }

  def nameLookup(methodName: Name, allClasses: Set[Symbol]): Set[Symbol] = {
    allClasses.collect {
      case cls => cls.tpe.members
    }.flatten.filter(m => m.name == methodName && m.isMethod)
  }

  override def initialize() {
    classes = trees.flatMap { tree =>
      tree.collect {
        case cd: ClassDef => cd.symbol
      }
    }.toSet

    trees.foreach { tree =>
      findCallSites(tree, List())
    }
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

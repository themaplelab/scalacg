package callgraph

import scala.tools.nsc

trait RA {
  this: CGUtils =>
  val global: nsc.Global

  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  def buildCallGraph() {
    for (callSite <- callSites) {
      val targets = nameLookup(callSite.staticTarget.name, classes)
      callGraph += (callSite -> targets)
    }
  }
  
  var instantiatedClasses = Set[Type]()
  var reachableCode = Set[Symbol]()
  var callbacks = Set[Symbol]() // todo (if necessary?)

  private def nameLookup(methodName: Name, allClasses: Set[Type]): Set[Symbol] = {
    allClasses.flatMap(_.members.filter(m => m.name == methodName && m.isMethod))
  }

  override def initialize() {
    classes = trees.flatMap { tree =>
      tree.collect {
        case cd: ClassDef => cd.symbol.tpe
      }
    }.toSet

    instantiatedClasses = classes
    reachableCode = classes.flatMap(_.members.filter(_.isMethod))
    
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

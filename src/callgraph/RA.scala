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
      reachableCode ++= targets
    }
  }
  
  var instantiatedClasses = Set[Type]()
  var reachableCode = Set[Symbol]()
  var callbacks = Set[Symbol]()

  private def nameLookup(methodName: Name, allClasses: Set[Type]): Set[Symbol] = {
    allClasses.flatMap(_.members.filter((m: Symbol) => m.name == methodName && m.isMethod))
  }

  override def initialize() {
    classes = trees.flatMap {
      _.collect {
        case cd: ClassDef => cd.symbol.tpe // todo: include ModuleDef?
      }
    }.toSet

    instantiatedClasses = classes

    reachableCode = mainMethods
    reachableCode ++= classes.map(_.typeSymbol.primaryConstructor)

    for {
      cls <- classes
      member <- cls.decls
      if member.isMethod && !member.isDeferred && member.allOverriddenSymbols.nonEmpty
      libraryOverriddenSymbols = member.allOverriddenSymbols.filterNot(appClasses contains _.enclClass)
      if libraryOverriddenSymbols.nonEmpty
    } {
      callbacks += member
    }

    trees.foreach { tree =>
      findCallSites(tree, List())
    }
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

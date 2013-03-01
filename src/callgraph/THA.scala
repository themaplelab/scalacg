package callgraph

import scala.tools.nsc

trait THA { this: CGUtils =>
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[MethodSymbol]]()

  def buildCallGraph = {
    def containingMethod(ancestors: List[Tree]): Symbol = {
      (for {
        firstDefDef <- ancestors.find(_.isInstanceOf[DefDef])
        instanceMethod <- firstDefDef.symbol.ownersIterator.find { sym =>
          sym.isMethod && sym.owner.isClass
        }
      } yield instanceMethod).getOrElse(NoSymbol)
    }
    for (callSite <- callSites) {
      val targets = callSite.receiver match {
        case ths: This =>
          val method = containingMethod(callSite.ancestors)
          if (method != NoSymbol && method.owner == ths.symbol)
            lookup(callSite.receiver.tpe, callSite.method, classes.filter(_.tpe.members.sorted.contains(method)))
          else
            lookup(callSite.receiver.tpe, callSite.method, classes)
        case _ =>
          lookup(callSite.receiver.tpe, callSite.method, classes)
      }
      callGraph += (callSite -> targets)
    }
  }
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

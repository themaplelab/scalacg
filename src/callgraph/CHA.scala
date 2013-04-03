package callgraph

import scala.tools.nsc

trait CHA { this: CGUtils =>
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  def buildCallGraph = {
    for (callSite <- callSites) {
      val targets = lookup(callSite.receiver.tpe, callSite.method, classes)
      callGraph += (callSite -> targets)
    }
  }
  
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

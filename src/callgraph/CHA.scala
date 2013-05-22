package callgraph

import scala.tools.nsc

trait CHA { this: CGUtils =>
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  def buildCallGraph = {
    for (callSite <- callSites) {
      if (callSite.receiver == null) {
        callGraph += (callSite -> Set(callSite.method))
      } else {
        val targets = lookup(callSite.receiver.tpe, callSite.method, classes)
        callGraph += (callSite -> targets)
      }
    }
  }

  // partial function -> if none of the cases apply, then it's undefined.
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

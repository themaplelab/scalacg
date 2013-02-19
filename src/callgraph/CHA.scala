package callgraph

import scala.tools.nsc

trait CHA { this: CGUtils =>
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[MethodSymbol]]()

  def buildCallGraph = {
    for (callSite <- callSites) {
      val targets = lookup(callSite.receiver.tpe, callSite.method, classes)
      callGraph += (callSite -> targets)
    }
  }
}

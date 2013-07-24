package callgraph

import analysis.AbstractAnalysis
import scala.tools.nsc

trait CHA { this: AbstractAnalysis =>
  val global: nsc.Global
  import global._

  def buildCallGraph() {
    for (callSite <- callSites) {
      if (callSite.receiver == null) {
        callGraph += (callSite -> Set(callSite.staticTarget))
      } else {
        val targets = lookup(callSite.staticTarget, allInstantiatedClasses, callSite.receiver.tpe)
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

package callgraph.analysis.util

import ca.uwaterloo.scalacg.util.Probe
import scala.Predef.Set

trait Lookup extends Probe {

  import global._

  /**
   * The main method lookup for Scala.
   */
  def lookup(staticTarget: MethodSymbol,
             consideredClasses: Set[Type],
             // default parameters, used only for super method lookup
             receiverType: Type = null,
             lookForSuperClasses: Boolean = false,
             getSuperName: (String => String) = (n: String) => n): Set[Symbol]
}

package callgraph.analysis.util

import scala.Predef.Set

trait Lookup extends Global {

  import global._

  /**
   * The main method lookup for Scala.
   */
  def lookup(staticTarget: MethodSymbol,
             consideredClasses: Set[Type],
             // default parameters, used only for super method lookup
             receiverType: Type = null,
             lookForSuperClasses: Boolean = false): Set[Symbol]
}

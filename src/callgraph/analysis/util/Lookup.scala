package callgraph.analysis.util

import scala.Predef.Set
import callgraph.analysis.TreeTraversal

trait Lookup extends Global {

  this: TreeTraversal =>

  import global._

  /**
   * The main method lookup for Scala.
   */
  def lookup(callSite: CallSite,
             consideredClasses: Set[Type],
             // default parameters, used only for super method lookup
             lookForSuperClasses: Boolean = false): Set[Symbol]
}

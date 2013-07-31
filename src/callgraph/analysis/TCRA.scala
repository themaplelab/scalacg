package callgraph.analysis


/* Type Composition Reachability Analysis */
trait TCRA extends RA with TCA {

  import global._

  override def lookup(callSite: CallSite,
                      consideredClasses: Set[Type],
                      lookForSuperClasses: Boolean = false) =
    super[RA].lookup(callSite, consideredClasses)
}

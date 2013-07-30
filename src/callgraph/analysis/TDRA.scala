package callgraph.analysis


trait TDRA extends RA with TCA {

  import global._

  override def lookup(callSite: CallSite,
                      consideredClasses: Set[Type],
                      lookForSuperClasses: Boolean = false) =
    super[RA].lookup(callSite, consideredClasses)
}

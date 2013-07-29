package callgraph.analysis


trait TDRA extends RA with TCA {

  import global._

  override def lookup(staticTarget: MethodSymbol,
                      consideredClasses: Set[Type],
                      // default parameters, used only for super method lookup
                      receiverType: Type = null,
                      lookForSuperClasses: Boolean = false,
                      getSuperName: (String => String) = (n: String) => n) =
    super[RA].lookup(staticTarget, consideredClasses)
}

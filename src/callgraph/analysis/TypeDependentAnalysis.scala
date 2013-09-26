package callgraph.analysis

import scala.collection.mutable

import callgraph.analysis.util.SuperCalls
import util.Lookup

trait TypeDependentAnalysis extends Lookup {

  this: AbstractAnalysis with SuperCalls =>

  import global._

  val concretization = mutable.Map[Symbol, Set[Type]]()

  // disabling the cache of lookups because it just consumes too much memory
  
  var cacheInstantiateTypeParams = Map[(Type, Type), Type]()

  private def lookupInClass(callSite: CallSite,
    tpe: Type,
    // default parameters, used only for super method lookup
    lookForSuperClasses: Boolean): Set[Symbol] = {

    val staticTarget = callSite.staticTarget
    val receiverType = callSite.receiver.tpe
    val tuple = (staticTarget, receiverType, tpe, lookForSuperClasses)
    var targets = List[Symbol]()
    for {
      expandedType <- expand(instantiateTypeParams(tpe, receiverType.widen))
      asf = expandedType.asSeenFrom(tpe, expandedType.typeSymbol)
      tpeasf = tpe.asSeenFrom(asf, tpe.typeSymbol)
      if tpeasf <:< asf || lookForSuperClasses
      targetName = staticTarget.name
      target: Symbol = if (lookForSuperClasses) {
        val targetString = targetName.toString
        val newName = if (lookForSuperClasses) superName(targetString) else targetString
        tpeasf.member(targetName.newName(newName))
      } else tpeasf.member(targetName)
      if target != NoSymbol && !target.isDeferred
    } {
      target match {
        case _ =>
          // Disambiguate overloaded methods based on the types of the args
          if (target.isOverloaded) {
            targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)) ::: targets
          } else {
            targets = target :: targets
          }
      }
    }
    val result = targets.toSet
    if (callSite.enclMethod.nameString == "main" && callSite.staticTarget.nameString == "m") println("targets: " + targets)
    result
  }

  override def lookup(callSite: CallSite,
    consideredClasses: collection.Set[Type],
    lookForSuperClasses: Boolean = false): collection.Set[Symbol] = {
    // If the target method is a constructor, no need to do the lookup.
    val staticTarget = callSite.staticTarget
    if (staticTarget.isConstructor)
      return Set(staticTarget)

    // If it's in the application, then resolve the call
    val targets = consideredClasses.flatMap(lookupInClass(callSite, _, lookForSuperClasses))

    /*
     * If the static target is in the application, return the set of resolved targets.
     * Else, return the set of resolved targets in addition to the static target. The static target then stands for
     * all those edges that we couldn't compute because we do not analyze the library.
     */
    (if (isLibrary(staticTarget)) targets + staticTarget else targets).toSet
  }

  private def expand(t: Type): Set[Type] = {
    val sym = t.typeSymbol
    if (sym.isAbstractType) {
      concretization.getOrElse(sym, Set())
    } else {
      Set(t)
    }
  }

  private def instantiateTypeParams(actual: Type, declared: Type): Type = {
    cacheInstantiateTypeParams.getOrElse((actual, declared), {
      val tparams = declared.typeArgs
      // Using `actual` rather than `ThisType(actual.typeSymbol)`, the latter causes loss of generic type information
      // see (Generics4)
      val args = tparams map {
        _.asSeenFrom(actual, declared.typeSymbol)
      }
      declared.instantiateTypeParams(tparams map {
        _.typeSymbol
      }, args)
    })
  }
}

package callgraph.analysis

import util.Lookup
import scala.Predef._

trait TypeDependentAnalysis extends Lookup {

  this: AbstractAnalysis =>

  import global._

  var concretization = Map[Symbol, Set[Type]]()

  override def getConsideredTypes = {
    trees.flatMap {
      tree =>
        tree.collect {
          case cd: ClassDef if cd.symbol.isModuleOrModuleClass => cd.symbol.tpe // isModuleClass -> an object, it gets auto instantiated
          case nw: New => nw.tpt.tpe // the set of all allocation sites
        }
    }.toSet
  }

  override def lookup(staticTarget: MethodSymbol,
             consideredClasses: Set[Type],
             // default parameters, used only for super method lookup
             receiverType: Type,
             lookForSuperClasses: Boolean = false,
             getSuperName: (String => String) = (n: String) => n): Set[Symbol] = {

    // If the target method is a constructor, no need to do the lookup.
    if (staticTarget.isConstructor)
      return Set(staticTarget)

    // If it's in the application, then resolve the call
    var targets = List[Symbol]()
    for {
      tpe <- consideredClasses
      expandedType <- expand(instantiateTypeParams(tpe, receiverType.widen))
      asf = expandedType.asSeenFrom(tpe, expandedType.typeSymbol)
      tpeasf = tpe.asSeenFrom(asf, tpe.typeSymbol)
      if tpeasf <:< asf || lookForSuperClasses
      target = if (lookForSuperClasses) {
        tpeasf.member(staticTarget.name.newName(getSuperName(staticTarget.name.toString)))
      } else tpeasf.member(staticTarget.name)
      if !target.isDeferred
    } {
      target match {
        case NoSymbol =>
          // TODO: can this ever happen? let's put in an assertion and see...
          assert(assertion = false, message = "tpe is " + tpe)

        case _ =>
          // Disambiguate overloaded methods based on the types of the args
          if (target.isOverloaded) {
            targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)) ::: targets
          } else {
            targets = target :: targets
          }
      }
    }

    /*
     * If the static target is in the application, return the set of resolved targets.
     * Else, return the set of resolved targets in addition to the static target. The static target then stands for
     * all those edges that we couldn't compute because we do not analyze the library.
     */
    if (isLibrary(staticTarget)) { // todo: what for super methods?
      targets = staticTarget :: targets
    }

    targets.toSet
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
    val tparams = declared.typeArgs
    // Using `actual` rather than `ThisType(actual.typeSymbol)`, the latter causes loss of generic type information
    // see (Generics4)
    val args = tparams map {
      _.asSeenFrom(actual, declared.typeSymbol)
    }
    declared.instantiateTypeParams(tparams map {
      _.typeSymbol
    }, args)
  }
}

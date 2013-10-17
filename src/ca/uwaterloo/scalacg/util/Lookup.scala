package ca.uwaterloo.scalacg.util

import scala.collection.mutable.Set

import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.config.Global

/**
 * All method resolution related operations go in here.
 */
trait Lookup {
  self: CallSites with Global with TypeConcretization with TypeOps =>

  import global._

  /**
   * Look for targets at the given call site in the given type.
   */
  def lookup(callSite: AbstractCallSite, tpe: Type) = {
    val targets = Set[Symbol]()

    for {
      expanded <- expand(instantiateTypeParams(tpe, callSite.receiver.widen))
      target = member(expanded, tpe, callSite.targetName)
      if target != NoSymbol && !target.isDeferred // a concrete member found.
    } {
      targets ++= getTargets(target, callSite.staticTarget)
    }

    targets
  }

  /**
   * Find a member method.
   */
  def member(expanded: Type, tpe: Type, targetName: Name) = {
    val (expandedAsSeenFromTpe, tpeAsSeenFromExpanded) = asSeenFrom(expanded, tpe)
    tpeAsSeenFromExpanded.member(targetName)
  }

  /**
   * Look for targets at the given call site using the given types. This is the normal method resolution that obeys
   * sub-typing relationships.
   */
  def lookup_<:<(callSite: AbstractCallSite, types: Set[Type]): Set[Symbol] = {
    val targets = Set[Symbol]()

    for (tpe <- types) {
      targets ++= lookup_<:<(callSite, tpe)
    }

    // Handle calls to the library.
    if (isLibrary(callSite.staticTarget)) {
      targets += callSite.staticTarget
    }

    targets
  }

  /**
   * Look for targets at the given call site in the given type, conforming to the sub-typing rules.
   */
  def lookup_<:<(callSite: AbstractCallSite, tpe: Type) = {
    val targets = Set[Symbol]()

    for {
      expanded <- expand(instantiateTypeParams(tpe, callSite.receiver.widen))
      target = member_<:<(expanded, tpe, callSite.targetName)
      if target != NoSymbol && !target.isDeferred // a concrete member found.
    } {
      targets ++= getTargets(target, callSite.staticTarget)
    }

    targets
  }

  /**
   * Find a member method in a super-type, as seen from the sub-type.
   */
  def member_<:<(expanded: Type, tpe: Type, targetName: Name) = {
    val (expandedAsSeenFromTpe, tpeAsSeenFromExpanded) = asSeenFrom(expanded, tpe)
    if (tpeAsSeenFromExpanded <:< expandedAsSeenFromTpe) tpeAsSeenFromExpanded.member(targetName)
    else NoSymbol
  }

  /**
   * Get all possible targets (including overloaded ones).
   */
  def getTargets(target: Symbol, staticTarget: Symbol) = {
    val targets = Set[Symbol]()
    target match {
      case _ =>
        // Disambiguate overloaded methods based on the types of the args
        if (target.isOverloaded) {
          targets ++= target.alternatives filter (_.tpe matches staticTarget.tpe)
        } else {
          targets += target
        }
    }
    targets
  }
  
  def lookupThis(callSite: CallSite, types: Set[Type]) = {
    
  }
}
package ca.uwaterloo.scalacg.util

import scala.collection.mutable.Set

import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.config.Global

trait SuperCalls {
  self: CallSites with Global with TypeOps with Lookup =>

  import global._

  /**
   * Lookup a super call. Calling super can have any of the following forms:
   * - super[T].bar: This gets translated into a call using "super" as a receiver, with T as the staticSuperReference.
   * - super.<init>: This also gets translated into a call using "super", but this needs no resolution
   * 				(just like any other constructor calls).
   * - super.bar: This gets translated into a call to super$bar, i.e., to a method with SUPERACCESSOR flag set. This
   * 				needs some logic to determine the name of the callee, then resolve that call.
   */
  def lookupSuper(callSite: AbstractCallSite, types: Set[Type]) = {
    assert(callSite.isSuperCall, "attempting to compute super calls for non-super call site.")

    val targets = Set[Symbol]()

    /*
     * If super[Z], lookup only Z, else it's call to the super constructor (i.e, don't resolve the call).
     * NOTE: Calls to the constructors are handled in processCallSites anyways.
     */
    if (callSite.hasSuperReceiver) {
      if (callSite.hasStaticSuperReference) {
        targets ++= lookup(callSite, callSite.staticSuperReference)
      } else {
        targets += callSite.staticTarget
      }
    } else { // If we're calling a method that has SUPERACCESSOR (e.g., super$bla)
      trimmedLinearizations(types, callSite.staticTarget.enclClass).foreach {
        linearization => targets ++= lookupFirst(callSite, linearization)
      }
    }

    targets
  }
  
  /**
   * Look for targets at the given call site in the given linearization. Stops after the first type
   * that returns a non-empty set of targets.
   */
  def lookupFirst(callSite: AbstractCallSite, linearization: List[Symbol]): Set[Symbol] = {
    for {
      cls <- linearization
      targets = lookup(callSite, cls.tpe)
    } {
      if (targets.nonEmpty) return targets
    }
    return Set.empty[Symbol]
  }

}
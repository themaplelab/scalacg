package callgraph.analysis

import scala.collection.immutable.Set
import scala.Predef._
import collection.mutable

trait RA extends WorklistAnalysis {

  import global._

  private val cacheCallsiteToTargets = mutable.Map[(Name, Boolean), collection.Set[Symbol]]()

  override def lookup(callSite: CallSite,
             consideredClasses: collection.Set[Type],
             // default parameters, used only for super method lookup
             lookForSuperClasses: Boolean = false): collection.Set[Symbol] = {
    val staticTarget = callSite.staticTarget

    // Don't lookup a call to a constructor
    if (staticTarget.isConstructor)
      return Set(staticTarget)

    val targetName = staticTarget.name
    val key = (targetName, lookForSuperClasses)

    // Do we have the result cached?
    if (!(cacheCallsiteToTargets contains key)) {
      // Lookup the targets by name
      val targets = consideredClasses.flatMap(_.members.filter((m: Symbol) =>
        m.name == (if (lookForSuperClasses) targetName.newName(superName(targetName.toString)) else targetName)
        && m.isMethod))

      // Add to cache
      cacheCallsiteToTargets += (key -> targets)
    }

    var ret = cacheCallsiteToTargets(key)
    // Add the static target if it's in the library
    if (isLibrary(staticTarget)) {
      ret += staticTarget
    }
    ret
  }

  override def buildCallGraph() {
    val callSitesSet = callSites.toSet
    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    addConstructorsToWorklist(types)
    addNewCallbacksToWorklist(types)

    while (methodWorklist.nonEmpty) {
      // Debugging info
      println("Items in work list: " + methodWorklist.size)
      processNewMethods(isTypeDependent = false)
      processCallSites(callSitesSet, types, isTypeDependent = false) // TODO: change types to newTypes
    }
    // Debugging info
    println("Work list is empty now.")
  }
}
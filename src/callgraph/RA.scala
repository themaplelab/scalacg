package callgraph

import analysis.WorklistAnalysis
import scala.collection.immutable.Set
import scala.Predef._

trait RA extends WorklistAnalysis {

  import global._

  private var cache = Map[(Name, Boolean), Set[Symbol]]()

  def getAllInstantiatedTypes: Set[Type] = {
    trees.flatMap {
      tree =>
        tree.collect {
          case cd: ClassDef => cd.symbol.tpe
          case nw: New => nw.tpt.tpe // to get the library types used in the application
        }
    }.toSet
  }

  def lookup(staticTarget: MethodSymbol,
             consideredClasses: Set[Type],
             // default parameters, used only for super method lookup
             receiverType: Type = null,
             lookForSuperClasses: Boolean = false,
             getSuperName: (String => String) = (n: String) => n): Set[Symbol] = {

    // Don't lookup a call to a constructor
    if (staticTarget.isConstructor)
      return Set(staticTarget)

    val key = (staticTarget.name, lookForSuperClasses)

    // Do we have the result cached?
    if (!(cache contains key)) {
      // Lookup the targets by name
      val targets = allInstantiatedTypes.flatMap(_.members.filter((m: Symbol) =>
        m.name == (if (lookForSuperClasses) staticTarget.name.newName(getSuperName(staticTarget.name.toString)) else staticTarget.name)
          && m.isMethod))

      // Add to cache
      cache += (key -> targets)
    }

    var ret = cache(key)
    // Add the static target if it's in the library
    if (isLibrary(staticTarget)) {
      ret += staticTarget
    }

    ret
  }

  def buildCallGraph() {

    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    addConstructorsToWorklist(allInstantiatedTypes)
    addNewCallbacksToWorklist(allInstantiatedTypes)

    while (methodWorklist.nonEmpty) {
      // Debugging info
      println("Items in work list: " + methodWorklist.size)
      processNewMethods(getClassesInMethod = false)
      processCallSites(allInstantiatedTypes, ra = true)
    }
    // Debugging info
    println("Work list is empty now.")
  }
}
package callgraph.analysis

import scala.collection.mutable

import util.SuperCalls

trait WorklistAnalysis extends AbstractAnalysis with SuperCalls {

  import global._

  val methodWorklist = mutable.Queue[Symbol]()

  val cacheProcessedMethods = mutable.Set[Symbol]()
  val cacheSuperCalls = mutable.Map[(MethodSymbol, Type), collection.Set[Symbol]]()

  def addMethod(method: Symbol) {
    if (!reachableCode(method) && !cacheProcessedMethods.contains(method)) {
      methodWorklist += method
      cacheProcessedMethods += method
    }
  }

  def addConstructorsToWorklist(types: collection.Set[Type]) {
    types.foreach((cls: Type) => {
      addMethod(cls.typeSymbol) // TODO: is that related to primary-constructor issues? i.e., code in ClassDef?
      cls.members.foreach((m: Symbol) => if (m.isConstructor) addMethod(m))
    })
  }

  def addNewCallbacksToWorklist(types: collection.Set[Type]) {
    for {
      cls <- types
      member <- cls.decls // loop over the declared members, "members" returns defined AND inherited members
      if isApplication(member) && isOverridingLibraryMethod(member)
    } {
      callbacks += member
      addMethod(member)
    }
  }

  /* isTypeDependent is true for TCA and false for RA: it indicates whether we're interested
   * in getting the instantiated classes inside of the new methods */
  def processNewMethods(isTypeDependent: Boolean) = {
    var newInstantiatedTypes = Set[Type]()
    var newReachableCode = Set[Symbol]()
    for (method <- methodWorklist.dequeueAll(_ => true)) {
      reachableCode += method
      newReachableCode += method
      if (isTypeDependent) {
        newInstantiatedTypes ++= classesInMethod(method)
      }
    }
    (newInstantiatedTypes, newReachableCode)
  }

  def findNewReachableCode = {
    var newReachableCode = mutable.Set[Symbol]()
    for (method <- methodWorklist.dequeueAll(_ => true)) {
      reachableCode += method
      newReachableCode += method
    }
    newReachableCode
  }

  def findNewInstantiatedTypes(instantiatedTypes: collection.Set[Type], methods: collection.Set[Symbol]) = {
    var newInstantiatedTypes = Set[Type]()

    for (method <- methods) {
      val typesInMethod = classesInMethod(method)
      newInstantiatedTypes ++= (typesInMethod -- instantiatedTypes) // remove types that were previously instantiated
    }

    newInstantiatedTypes = findDuplicates(newInstantiatedTypes)
    newInstantiatedTypes
  }

  def getApply(tpe: Type) = {
    assert(tpe.typeSymbol.isAnonymousFunction, "Finding an apply method for non-anonymous function: " + tpe)
    tpe.members.filter(_.nameString == "apply").head
  }

  def findDuplicates(types: Set[Type]): Set[Type] = {
    var filtered = Set[Type]()

    // Helper class to provide a containsType method for Set[Type]
    class SetHelper(types: Set[Type]) {
      def containsType(that: Type): Boolean = {
        for (tpe <- types) {
          if (tpe.typeSymbol.isAnonymousFunction && that.typeSymbol.isAnonymousFunction) {
            val a1 = getApply(tpe)
            val a2 = getApply(that)
            val body1 = methodToBody(a1)
            val body2 = methodToBody(a2)
            if (body1 equalsStructure body2) {
              return true
            }
          }
        }
        return false
      }
    }
    implicit def setWrapper(types: Set[Type]) = new SetHelper(types)

    for (tpe <- types) {
      if (!filtered.containsType(tpe)) {
        filtered += tpe
      }
    }

    return filtered
  }

  def findCallSites(code: collection.Set[Symbol]) = {
    callSites.filter(code contains _.enclMethod).toSet
  }

  def processCallSites(callSites: collection.Set[CallSite], types: collection.Set[Type],
    isTypeDependent: Boolean,
    getFilteredClasses: (CallSite, collection.Set[Type]) => collection.Set[Type] = (_, _) => Set()) {
    for (callSite <- callSites) {
      var targets = collection.Set[Symbol]()

      if (callSite.receiver == null) {
        targets = Set(callSite.staticTarget)
      } else if (isSuperCall(callSite)) {
        targets = cacheSuperCalls.getOrElseUpdate((callSite.staticTarget, callSite.receiver.tpe), getSuperTargets(callSite, classToContainedInLinearizationOf(callSite.enclMethod.enclClass.tpe), isTypeDependent))
      } else {
        val classesToLookup: collection.Set[Type] = if (isTypeDependent) getFilteredClasses(callSite, types) else types
        targets = lookup(callSite, classesToLookup)
      }

      callGraph += (callSite -> (callGraph.getOrElse(callSite, Set()) ++ targets))
      targets.foreach(addMethod)
    }
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

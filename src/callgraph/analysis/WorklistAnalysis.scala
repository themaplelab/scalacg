package callgraph.analysis

import collection.mutable
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
    
    for(method <- methods) {
      val typesInMethod = classesInMethod(method)
      newInstantiatedTypes ++= (typesInMethod -- instantiatedTypes) // remove types that were previously instantiated
    }
    
    newInstantiatedTypes
  }
  
  def findCallSites(code: collection.Set[Symbol]) = {
    callSites.filter(code contains _.enclMethod).toSet
  }

  def processCallSites(callSites: collection.Set[CallSite], types: collection.Set[Type],
    isTypeDependent: Boolean,
    getFilteredClasses: CallSite => collection.Set[Type] = (_ => Set())) {
    for(callSite <- callSites) {
      val csStaticTarget = callSite.staticTarget
      val receiver = callSite.receiver
      var targets = collection.Set[Symbol]()

      if (receiver == null) {
        targets = Set(csStaticTarget)
      } else if (isSuperCall(callSite)) {
        targets = cacheSuperCalls.getOrElseUpdate((callSite.staticTarget, callSite.receiver.tpe), getSuperTargets(callSite, classToContainedInLinearizationOf(callSite.enclMethod.enclClass.tpe), isTypeDependent))
      } else {
        val classesToLookup: collection.Set[Type] = if (isTypeDependent) getFilteredClasses(callSite) else types
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

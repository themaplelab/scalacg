package callgraph.analysis

import collection.mutable
import util.SuperCalls

trait WorklistAnalysis extends AbstractAnalysis with SuperCalls {

  import global._

  val methodWorklist = mutable.Queue[Symbol]()

  var cacheProcessedMethods = Set[Symbol]()

  def addMethod(method: Symbol) {
    if (!reachableCode(method) && !cacheProcessedMethods.contains(method)) {
      methodWorklist += method
      cacheProcessedMethods += method
    }
  }

  var cacheClassesForProcessedConstructors = Set[Type]()

  def addConstructorsToWorklist(classes: Set[Type]) {
    classes.foreach((cls: Type) => {
      if (!cacheClassesForProcessedConstructors.contains(cls)) {
        addMethod(cls.typeSymbol)
        cls.members.foreach((m: Symbol) => if (m.isConstructor) addMethod(m))
        cacheClassesForProcessedConstructors += cls
      }
    })
  }

  def addNewCallbacksToWorklist(classes: Set[Type]) {
    for {
      cls <- classes
      member <- cls.decls // loop over the declared members, "members" returns defined AND inherited members
      if isApplication(member) && isOverridingLibraryMethod(member)
    } {
      callbacks += member
      addMethod(member)
    }
  }

  /* isTypeDependent is true for TCA and false for RA: it indicates whether we're interested
   * in getting the instantiated classes inside of the new methods */
  def processNewMethods(isTypeDependent: Boolean): Set[Type] = {
    var newInstantiatedTypes = Set[Type]()
    for (method <- methodWorklist.dequeueAll(_ => true)) {
      reachableCode += method
      if (isTypeDependent) {
        newInstantiatedTypes ++= classesInMethod(method)
      }
    }
    newInstantiatedTypes
  }

  def processCallSites(types: Set[Type],
                       newTypes: Set[Type],
                       isTypeDependent: Boolean,
                       getFilteredClasses: CallSite => Set[Type] = (_ => Set())) {
    for {
      callSite <- callSites
      if reachableCode contains callSite.enclMethod
    } {
      val csStaticTarget = callSite.staticTarget
      val receiver = callSite.receiver
      var targets = Set[Symbol]()

      if (receiver == null) {
        targets = Set(csStaticTarget)
      } else {
        val classesToLookup: Set[Type] = if (isTypeDependent) getFilteredClasses(callSite) else newTypes
        val (superTargets, isConcreteSuper) = getSuperTargets(callSite, types, isTypeDependent)
        if (isConcreteSuper && isTypeDependent)
          targets = superTargets
        else
          targets = lookup(callSite, classesToLookup) ++ superTargets
      }

      callGraph += (callSite -> targets)
      targets.foreach(addMethod)
    }
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

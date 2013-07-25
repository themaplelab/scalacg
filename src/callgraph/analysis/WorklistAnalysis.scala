package callgraph.analysis

import collection.mutable
import util.SuperCalls

trait WorklistAnalysis extends AbstractAnalysis with SuperCalls {

  import global._

  // newly reachable methods to be processed
  val methodWorklist = mutable.Queue[Symbol]()

  def addMethod(method: Symbol) = if (!reachableCode(method)) methodWorklist += method

  def addConstructorsToWorklist(classes: Set[Type]) {
    classes.map(_.typeSymbol).foreach(addMethod)
    for {
      cls <- classes
      constr <- cls.members
      if constr.isConstructor
    } {
      addMethod(constr)
    }
  }

  def addNewCallbacksToWorklist(classes: Set[Type]) {
    for {
      cls <- classes
      member <- cls.decls // loop over the declared members, "members" returns defined AND inherited members
      if isApplication(member) && isOverridingLibraryMethod(member)
    } {
      callbacks += member
    }
    callbacks.foreach(addMethod)
  }

  def processNewMethods(getClassesInMethod: Boolean): Set[Type] = {
    var soFarInstantiatedClasses = Set[Type]()
    for (method <- methodWorklist.dequeueAll(_ => true)) {
      reachableCode += method
      if (getClassesInMethod) {
        soFarInstantiatedClasses ++= classesInMethod(method)
      }
    }
    soFarInstantiatedClasses
  }

  def processCallSites(classes: Set[Type],
                       ra: Boolean,
                       getFilteredClasses: CallSite => Set[Type] = (_ => Set())) {
    // process all call sites in reachable code
    for {
      callSite <- callSites
      if reachableCode contains callSite.enclMethod
    } {
      val csStaticTarget = callSite.staticTarget
      var targets = Set[Symbol]()

      if (callSite.receiver == null) {
        targets = Set(csStaticTarget)
      } else {
        val classesToLookup: Set[Type] = if (ra) classes else getFilteredClasses(callSite)
        val superTargets = getSuperTargets(callSite, classes, ra)
        targets = lookup(csStaticTarget, classesToLookup, callSite.receiver.tpe) ++ superTargets // todo: for super[T] lookup here not necessary
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

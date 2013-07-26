package callgraph

import analysis.{InstantiationDependentAnalysis, WorklistAnalysis}

trait RTA extends WorklistAnalysis with InstantiationDependentAnalysis {

  import global._
  
  def buildCallGraph() {
    var soFarInstantiatedClasses = Set[Type]()
    // all objects are considered to be allocated
    soFarInstantiatedClasses ++= allInstantiatedTypes.filter(_.typeSymbol.isModuleOrModuleClass)
    // this should be the same as in AbstractAnalysis.initialize
    // so this probably should say isModuleClass, if it breaks, then revert :D
    
    methodWorklist ++= entryPoints

    while (!methodWorklist.isEmpty) {
      // process new methods
      for (method <- methodWorklist.dequeueAll(_ => true)) {
        reachableCode += method
        soFarInstantiatedClasses ++= classesInMethod(method)
      }

      // process all call sites
      for (callSite <- callSites) {
        val targets = lookup(callSite.staticTarget, soFarInstantiatedClasses, callSite.receiver.tpe)
        callGraph += (callSite -> targets)
        targets.foreach(addMethod)
      }

      // add all constructors
      soFarInstantiatedClasses.map(_.typeSymbol).foreach(addMethod)
      for {
        tpe <- soFarInstantiatedClasses
        constr <- tpe.members
        if constr.isConstructor
      } addMethod(constr)
    }
  }
  override val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
  }
}
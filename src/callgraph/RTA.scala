package callgraph

import scala.collection.mutable

trait RTA { this: AbstractAnalysis =>

  import global._

  // in Scala, code can appear in classes as well as in methods, so reachableCode generalizes reachable methods
  var reachableCode = Set[Symbol]()

  // newly reachable methods to be processed
  val methodQueue = mutable.Queue[Symbol]()
  def addMethod(method: Symbol) = {
    if (!reachableCode(method)) methodQueue += method
  }
  
  // the set of classes instantiated in a given method
  lazy val classesInMethod = {
    val ret = mutable.Map[Symbol, Set[Type]]().withDefaultValue(Set())
    def traverse(tree: Tree, owner: Symbol): Unit = {
      tree match {
        case _: ClassDef | _: DefDef =>
          tree.children.foreach(traverse(_, tree.symbol))
        case New(tpt) =>
          ret(owner) = ret(owner) + tpt.tpe.dealias // some types are aliased, see CaseClass3
        case _ =>
          tree.children.foreach(traverse(_, owner))
      }
    }
    trees.foreach(traverse(_, NoSymbol))
    ret
  }
  
  var callGraph = Map[CallSite, Set[Symbol]]()

  def buildCallGraph = {
    // all objects are considered to be allocated
    instantiatedClasses ++= classes.filter(_.typeSymbol.isModuleOrModuleClass) 
    // this should be the same as in AbstractAnalysis.initialize
    // so this probably should say isModuleClass, if it breaks, then revert :D
    
    
    methodQueue ++= entryPoints

    while (!methodQueue.isEmpty) {
      // process new methods
      for (method <- methodQueue.dequeueAll(_ => true)) {
        reachableCode += method
        instantiatedClasses ++= classesInMethod(method)
      }

      // process all call sites
      for (callSite <- callSites) {
        val targets = lookup(callSite.staticTarget, instantiatedClasses, callSite.receiver.tpe)
        callGraph += (callSite -> targets)
        targets.foreach(addMethod)
      }

      // add all constructors
      instantiatedClasses.map(_.typeSymbol).foreach(addMethod)
      for {
        tpe <- instantiatedClasses
        constr <- tpe.members
        if constr.isConstructor
      } addMethod(constr)
    }
  }
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
  }
}
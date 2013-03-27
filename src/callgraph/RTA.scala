package callgraph

import scala.tools.nsc
import scala.collection.mutable

trait RTA { this: CGUtils =>
  val global: nsc.Global
  import global._

  var instantiatedClasses = Set[ClassSymbol]()
  // in Scala, code can appear in classes as well as in methods, so reachableCode generalizes reachable methods
  var reachableCode = Set[Symbol]()

  // newly reachable methods to be processed
  val methodQueue = mutable.Queue[Symbol]()
  def addMethod(method: Symbol) = {
    if (!reachableCode(method)) methodQueue += method
  }
  
  // the set of classes instantiated in a given method
  lazy val classesInMethod = {
    val ret = mutable.Map[Symbol, Set[ClassSymbol]]().withDefaultValue(Set())
    def traverse(tree: Tree, owner: Symbol): Unit = {
      tree match {
        case _: ClassDef | _: DefDef =>
          tree.children.foreach(traverse(_, tree.symbol))
        case New(tpt) =>
          ret(owner) = ret(owner) + tpt.symbol.asClass
        case _ =>
          tree.children.foreach(traverse(_, owner))
      }
    }
    trees.foreach(traverse(_, NoSymbol))
    ret
  }
  
  var callGraph = Map[CallSite, Set[MethodSymbol]]()

  def buildCallGraph = {
    // all objects are considered to be allocated
    instantiatedClasses ++= classes.filter(_.isModule)

    methodQueue ++= entryPoints

    while (!methodQueue.isEmpty) {
      // process new methods
      for (method <- methodQueue.dequeueAll(_ => true)) {
        reachableCode += method
        instantiatedClasses ++= classesInMethod(method)
      }

      // process all call sites
      for (callSite <- callSites) {
        val targets = lookup(callSite.receiver.tpe, callSite.method, instantiatedClasses)
        callGraph += (callSite -> targets)
        targets.foreach(addMethod(_))
      }

      // add all constructors
      instantiatedClasses.foreach(addMethod(_))
      for {
        cls <- instantiatedClasses
        constr <- cls.tpe.members
        if constr.isConstructor
      } addMethod(constr)
    }
  }
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
  }
}
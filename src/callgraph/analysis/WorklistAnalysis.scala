package callgraph.analysis

import collection.mutable

trait WorklistAnalysis extends AbstractAnalysis {

  import global._

  // newly reachable methods to be processed
  val methodWorklist = mutable.Queue[Symbol]()

  def addMethod(method: Symbol) = if (!reachableCode(method)) methodWorklist += method
}

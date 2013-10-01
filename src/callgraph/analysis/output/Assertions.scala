package callgraph.analysis.output

import Predef._
import scala.collection.mutable

trait Assertions extends Probe {

  import global._

  /**
   * Are all the methods that have @reachable annotation, reachable in the call graph
   */
  def assertReachables(expectedReachables: mutable.Set[Symbol], reachableMethods: Set[Symbol]) {
    if (!expectedReachables.isEmpty) {
      println("Expected reachables: " + expectedReachables.map(signature).toSeq.mkString(", "))
      println("Reachable methods: " + reachableMethods.map(signature).toSeq.mkString(", "))
      assert(expectedReachables.subsetOf(reachableMethods),
        "Some methods are annotated with @reachable, but are not reachable in the call graph.\n" + 
        (expectedReachables -- reachableMethods).map(signature).toSeq.mkString(", "))
    }
  }
  
  /**
   * Check that methods annotated with @notreachable are not present in the call graph
   */
  def assertNotReachables(expectedNotReachables : mutable.Set[Symbol], reachableMethods: Set[Symbol]) {
    if (!expectedNotReachables.isEmpty) {
      println("Expected notReachables: " + expectedNotReachables.map(signature).toSeq.mkString(", "))
      println("Reachable methods: " + reachableMethods.map(signature).toSeq.mkString(", "))
      assert(expectedNotReachables.intersect(reachableMethods).isEmpty,
        "Some methods are annotated with @notreachable, but are reachable in the call graph.")
    } 
  }
}
package ca.uwaterloo.scalacg.util

import scala.tools.nsc.Global

trait Assertions extends Probe {

  val global: Global
  import global._

  /**
   * Are all the methods that have @reachable annotation, reachable in the call graph
   */
  def assertReachables(expectedReachables: Set[Symbol], reachableMethods: Set[Symbol]) {
    if (!expectedReachables.isEmpty) {
      println("Expected reachables: " + expectedReachables.map(probeMethod).toSeq.mkString(", "))
      println("Reachable methods: " + reachableMethods.map(probeMethod).toSeq.mkString(", "))
      assert(expectedReachables.subsetOf(reachableMethods),
        "Some methods are annotated with @reachable, but are not reachable in the call graph.")
    }
  }

}
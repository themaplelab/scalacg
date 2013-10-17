package ca.uwaterloo.scalacg.util

import ca.uwaterloo.scalacg.analysis.CallGraphAnalysis

trait Assertions extends Probe {
  self: CallGraphAnalysis with Annotations =>

  import global._

  /**
   * Do all the defined assertions.
   */
  def doAssertions = {
    assertReachables
    assertNotReachables
    assertReceiverAnnotations
    assertInvocationsAnnotations
  }

  /**
   * Are all the methods that have @reachable annotation present in the call graph?
   */
  def assertReachables = {
    if (!expectedReachables.isEmpty) {
      println("Expected reachables: " + expectedReachables.map(signature).toSeq.mkString(", "))
      println("Reachable methods: " + reachableMethods.reachableItems.map(signature).toSeq.mkString(", "))
      assert(expectedReachables.subsetOf(reachableMethods.reachableItems),
        "Some methods are annotated with @reachable, but are not reachable in the call graph.\n" +
          (expectedReachables -- reachableMethods.reachableItems).map(signature).toSeq.mkString(", "))
    }
  }

  /**
   * Are all the methods annotated with @notreachable not present in the call graph?
   */
  def assertNotReachables = {
    if (!expectedNotReachables.isEmpty) {
      println("Expected notReachables: " + expectedNotReachables.map(signature).toSeq.mkString(", "))
      println("Reachable methods: " + reachableMethods.reachableItems.map(signature).toSeq.mkString(", "))
      assert(expectedNotReachables.intersect(reachableMethods.reachableItems).isEmpty,
        "Some methods are annotated with @notreachable, but are reachable in the call graph.\n" +
          (expectedNotReachables & reachableMethods.reachableItems).map(signature).toSeq.mkString(", "))
    }
  }

  /**
   * Are all the specified receiver annotations present in the call graph?
   */
  def assertReceiverAnnotations = {
    for {
      abstractCallSite <- callSites.reachableItems
      callSite <- abstractToCallSites(abstractCallSite)
      expected = callSite.annotations.toSet
      if expected.nonEmpty
    } {
      val resolved = callGraph(callSite).map(findTargetAnnotations)
      println("Resolved: " + resolved.toSeq.sorted.mkString(", "))
      println("Expected: " + expected.toSeq.sorted.mkString(", "))
      assert(resolved == expected, expected.toSeq.sorted.mkString(", "))
    }
  }

  /**
   * Are all the specified @invocations present in the call graph?
   */
  def assertInvocationsAnnotations = {
    val symbols = reachableMethods.reachableItems ++ types.map(_.typeSymbol)

    for {
      symbol <- symbols
      hasNoInvocations = hasNoInvocationsAnnotation(symbol)
      expected = findInvocationsAnnotations(symbol)
      if expected.nonEmpty || hasNoInvocations
    } {
      assert(expected.nonEmpty != hasNoInvocations, "@invocations should not be combined with @noInvocations for the same symbol")
      val method = if (symbol.isMethod) symbol else symbol.primaryConstructor
      val abstractCallSites = callSitesInMethod(method)

      val resolved = abstractCallSites.flatMap { acs =>
        abstractToCallSites(acs).flatMap { callSite =>
          callGraph(callSite).map(callSite.position.line + ": " + findTargetAnnotations(_))
        }
      }

      println("Resolved: " + resolved.toSeq.sorted.mkString(", "))
      println("Expected: " + expected.toSeq.sorted.mkString(", "))
      assert(if (hasNoInvocations) resolved.isEmpty else expected.subsetOf(resolved),
        (expected &~ resolved).toSeq.sorted.mkString(", "))
    }
  }
}
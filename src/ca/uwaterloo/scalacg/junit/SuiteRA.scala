package ca.uwaterloo.scalacg.junit

import org.junit.Test

/*
* Reachability analysis tests
*
* In order to run the tests, mix-in the RA analysis in
*   callgraph.CallGraphPlugin.CallGraphPhase
* (the existing analysis class, e.g. TCA, should be replaced by RA)
*
* I.e. the declaration of the class should look like:
*   class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with AbstractAnalysis with Assertions with RA with Probe { ... }
* */

class SuiteRA extends Suite("junit/src/tests/ra/", List("callgraph:ra-all", "callgraph:assert")) {

  @Test def testRaTest() { runTest("RATest") }

  @Test def testRaCallbacks() { runTest("RACallbacks") }

  @Test def testAnnotations1() { runTest("Annotations1") }

  @Test def testAnnotations2() { runTest("Annotations2") }

  @Test def testGenerics1() { runTest("Generics1") }

  @Test def testAbstractTypes1() { runTest("AbstractTypes1") }

  @Test def testApply1() { runTest("Apply1") }

  @Test def testBreakable() { runTest("Breakable") }

  @Test def testCaseClass1() { runTest("CaseClass1") }

  @Test def testClosures1() { runTest("Closures1") }

  @Test def testForeach1() { runTest("Foreach1") }

  @Test def testGetterMethod1() { runTest("GetterMethod1") }

  @Test def testImplicitArguments1() { runTest("ImplicitArguments1") }

  @Test def testImplicits1() { runTest("Implicits1") }

  @Test def testInfix() { runTest("Infix") }

  @Test def testLibraryCall() { runTest("LibraryCall") }

  @Test def testMatch1() { runTest("Match1") }

  @Test def testOperator1() { runTest("Operator1") }

  @Test def testPathTypes1() { runTest("PathTypes1") }

  @Test def testThisType1() { runTest("ThisType1") }

  @Test def testToString1() { runTest("ToString1") }

  @Test def testTraits1() { runTest("Traits1") }

  @Test def testValOverridesMethod() { runTest("ValOverridesMethod") }
}

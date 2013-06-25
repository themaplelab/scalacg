package test

import org.junit.Test

/*
* Reachability analysis tests
*
* In order to run the tests, mix-in the RA analysis in
*   callgraph.CallGraphPlugin.CallGraphPhase
* (the existing analysis class, e.g. THA, should be replaced by RA)
*
* I.e. the declaration of the class should look like:
*   class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with CGUtils with Assertions with RA with Probe { ... }
* */

class SuiteRA {
  def runTest(filename: String) {
    new Suite().runTest(filename)
  }
  
  @Test def testRaTest() {
    runTest("ra/RATest")
  }
  
  @Test def testRaCallbacks() {
    runTest("ra/RACallbacks")
  }
  
  @Test def testAnnotations1() {
    runTest("ra/Annotations1")
  }

  @Test def testAnnotations2() {
    runTest("ra/Annotations2")
  }

  @Test def testGenerics1() {
    runTest("ra/Generics1")
  }
  
  @Test def testAbstractTypes1() {
    runTest("ra/AbstractTypes1")
  }
  
  @Test def testApply1() {
    runTest("ra/Apply1")
  }
  
  @Test def testBreakable() {
    runTest("ra/Breakable")
  }
  
  @Test def testCaseClass1() {
    runTest("ra/CaseClass1")
  }
  
  @Test def testClosures1() {
    runTest("ra/Closures1")
  }
  
  @Test def testForeach1() {
    runTest("ra/Foreach1")
  }
  
  @Test def testGetterMethod1() {
    runTest("ra/GetterMethod1")
  }
  
  @Test def testImplicitArguments1() {
    runTest("ra/ImplicitArguments1")
  }
  
  @Test def testImplicits1() {
    runTest("ra/Implicits1")
  }
  
  @Test def testInfix() {
    runTest("ra/Infix")
  }
  
//  @Test def testLibraryCall() {   // todo: add when library calls are done
//    runTest("ra/LibraryCall")
//  }
  
//  @Test def testMatch1() {        // todo: add later
//    runTest("ra/Match1")
//  }
  
  @Test def testOperator1() {
    runTest("ra/Operator1")
  }
  
  @Test def testOverriding() {
    runTest("ra/Overriding")
  }
  
  @Test def testPathTypes1() {
    runTest("ra/PathTypes1")
  }
  
  @Test def testThisType1() {
    runTest("ra/ThisType1")
  }
  
//  @Test def testToString1() {		// todo: add when library calls are done
//    runTest("ra/ToString1")
//  }
  
  @Test def testTraits1() {
    runTest("ra/Traits1")
  }
  
  @Test def testValOverridesMethod() {
    runTest("ra/ValOverridesMethod")
  }
}

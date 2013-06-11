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
}

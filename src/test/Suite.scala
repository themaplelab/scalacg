package test

import org.junit.Test
import scala.tools.nsc

class Suite {
  def runTest(filename: String) = {
    val settings = new nsc.Settings
    settings.d.value = "tests/classes"
    settings.plugin.value = List("bin/callgraph-plugin.jar")
    val g = new nsc.Global(settings)
    new (g.Run).compile(List("tests/" + filename + ".scala", "tests/target.scala"))
  }

  @Test def testTest1 = runTest("Test1")
  @Test def testTest2 = runTest("Test2")
  @Test def testTest3 = runTest("Test3")
  
   @Test def testTraits1 = runTest("Traits1")
   @Test def testOverriding1 = runTest("Overriding1")
}

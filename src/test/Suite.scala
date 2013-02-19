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
}

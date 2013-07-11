package test

import tools.nsc
import org.junit.Test

class SuiteIntegr {

  def runTest(filename: String) {
    val settings = new nsc.Settings
    settings.d.value = "integration/bin"
    settings.plugin.value = List("jar/callgraph-plugin.jar")
    settings.bootclasspath.append("lib/scala-2.10.2/scala-compiler.jar")
    settings.bootclasspath.append("lib/scala-2.10.2/scala-library.jar")
    settings.bootclasspath.append("lib/scala-2.10.2/scala-reflect.jar")
    settings.bootclasspath.append("integration/bin")
    settings.bootclasspath.append("bin")
    val g = new nsc.Global(settings)
    println("==============================")
    println(filename)
    println("==============================")
    try {
      new (g.Run).compile(List("integration/src/tests/" + filename + ".scala",
        "src/callgraph/annotation/target.scala",
        "src/callgraph/annotation/invocations.scala"))
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        throw ex
    }
    println("")
  }

  @Test def testTicTacToe() {
    runTest("TicTacToe")
  }
}

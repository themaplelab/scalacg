package test

import scala.tools.nsc

abstract class Suite(testPath: String, options: List[String] = Nil) {

  def runTest(filename: String) {
    val settings = new nsc.Settings
    settings.d.value = "junit/bin"
    settings.plugin.value = List("jar/callgraph-plugin.jar")
    settings.bootclasspath.append("lib/scala-2.10.1/scala-compiler.jar")
    settings.bootclasspath.append("lib/scala-2.10.1/scala-library.jar")
    settings.bootclasspath.append("lib/scala-2.10.1/scala-reflect.jar")
    settings.bootclasspath.append("junit/bin")
    settings.bootclasspath.append("bin")
    settings.pluginOptions.value = options
    //    settings.YmacrodebugVerbose.value = true
    //    settings.debug.value = true
    //    settings.Xprint.value = List("callgraph")
    settings.nowarn.value = true

    val g = new nsc.Global(settings)
    println("==============================")
    println(filename)
    println("==============================")
    try {
      new (g.Run).compile(List(testPath + filename + ".scala",
        "src/callgraph/annotation/target.scala",
        "src/callgraph/annotation/invocations.scala"))
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        throw ex
    }
    println()
  }
}

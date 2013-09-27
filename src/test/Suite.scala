package test

import scala.tools.nsc
import scala.util.Properties

abstract class Suite(testPath: String, options: List[String] = Nil) {
  private final val version2_10_0 = "2.10.0"
  private final val version2_10_2 = "2.10.2"

  def runTest(filename: String) {
    val settings = new nsc.Settings

    settings.d.value = "junit/bin" // setting output directory
    settings.plugin.value = List("jar/callgraph-plugin.jar") // adding plugin
    settings.nowarn.value = true // ignore warning, it just clutters the output

    // setting the scala version to be used
    //    val scalaVersion = if (Properties.versionNumberString > version2_10_0) version2_10_2 else version2_10_0
    //    settings.bootclasspath.value = (s"lib/scala-${scalaVersion}/lib/scala-compiler.jar")
    //    settings.bootclasspath.append(s"lib/scala-${scalaVersion}/lib/scala-library.jar")
    //    settings.bootclasspath.append(s"lib/scala-${scalaVersion}/lib/scala-reflect.jar")

    // add some other stuff to the class path
    settings.bootclasspath.append("junit/bin")
    settings.bootclasspath.append("bin")

    // setup the plugin-specific options
    settings.pluginOptions.value = options

    //    settings.YmacrodebugVerbose.value = true
    //    settings.debug.value = true
    //    settings.Xprint.value = List("callgraph")

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

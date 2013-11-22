package ca.uwaterloo.scalacg.junit

import scala.tools.nsc
import ca.uwaterloo.scalacg.config.GlobalConstants

abstract class Suite(var testPath: String, options: List[String] = Nil) extends GlobalConstants {

  def runTest(filename: String) {
    val settings = new nsc.Settings

    settings.d.value = "junit/bin"
    settings.nowarn.value = true

    settings.plugin.value = List("jar/callgraph-plugin.jar")

    //    settings.bootclasspath.append("lib/scala-2.10.1/scala-compiler.jar")
    //    settings.bootclasspath.append("lib/scala-2.10.1/scala-library.jar")
    //    settings.bootclasspath.append("lib/scala-2.10.1/scala-reflect.jar")
    settings.bootclasspath.append("junit/bin")
    settings.bootclasspath.append("bin")

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
        annotationPackageSrc + "/target.scala",
        annotationPackageSrc + "/invocations.scala"))
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        throw ex
    }
    println()
  }
}

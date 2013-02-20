package test

import org.junit.Test
import scala.tools.nsc
import org.junit.Assert

class Suite {
  def runTest(filename: String) = {
    val settings = new nsc.Settings
    settings.d.value = "junit/bin"
    settings.plugin.value = List("jar/callgraph-plugin.jar")
    settings.bootclasspath.append("junit/bin")
    val g = new nsc.Global(settings)
    println("==============================")
    println(filename)
    println("==============================")
    try {
      new (g.Run).compile(List("junit/src/tests/" + filename + ".scala", "junit/src/tests/target.scala"))
    } catch {case ex: Throwable =>
      ex.printStackTrace
      throw ex
    }
    println("")
  }
  
//  @Test def testTest1 = runTest("Test1")
//  @Test def testTest2 = runTest("Test2")
//  @Test def testTest3 = runTest("Test3")
//
//  @Test def testTraits1 = runTest("Traits1")
//  @Test def testTraits2 = runTest("Traits2")
//  @Test def testTraits3 = runTest("Traits3")
//
//  @Test def testOverriding1 = runTest("Overriding1")
  @Test def testOverloading = runTest("Overloading")

//  @Test def testClosures1 = runTest("Closures1")
//  @Test def testClosures2 = runTest("Closures2")
//  
//  @Test def testPathTypes1 = runTest("PathTypes1") 
//  @Test def testPathTypes2 = runTest("PathTypes2") 
//  @Test def testPathTypes3 = runTest("PathTypes3") 
}

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
  
  @Test def testTest1 = runTest("Test1")
  @Test def testTest2 = runTest("Test2")
  @Test def testTest3 = runTest("Test3")

  @Test def testTraits1 = runTest("Traits1")
  @Test def testTraits2 = runTest("Traits2")
  @Test def testTraits3 = runTest("Traits3")
  @Test def testTraits4 = runTest("Traits4")
  @Test def testTraits5 = runTest("Traits5")
  @Test def testTraits6 = runTest("Traits6")
  @Test def testTraits7 = runTest("Traits7")
  @Test def testTraits8 = runTest("Traits8")
  @Test def testTraits9 = runTest("Traits9")
  @Test def testTraits10 = runTest("Traits10")
  
  @Test def testThisType1 = runTest("ThisType1")
  @Test def testThisType2 = runTest("ThisType2")
  @Test def testThisType3 = runTest("ThisType3")

  @Test def testOverriding1 = runTest("Overriding1")
  @Test def testOverloading = runTest("Overloading")
  
  @Test def testSimple = runTest("Simple")

  @Test def testClosures1 = runTest("Closures1")
  @Test def testClosures2 = runTest("Closures2")
  
  @Test def testPathTypes1 = runTest("PathTypes1") 
  @Test def testPathTypes2 = runTest("PathTypes2") 
  @Test def testPathTypes3 = runTest("PathTypes3") 
  
  @Test def testAbstractTypes1 = runTest("AbstractTypes1") 
  @Test def testAbstractTypes2 = runTest("AbstractTypes2") 
  @Test def testAbstractTypes3 = runTest("AbstractTypes3") 
  @Test def testAbstractTypes4 = runTest("AbstractTypes4") 
  @Test def testAbstractTypes5 = runTest("AbstractTypes5") 
  @Test def testAbstractTypes6 = runTest("AbstractTypes6") 
  @Test def testAbstractTypes7 = runTest("AbstractTypes7") 
  @Test def testAbstractTypes8 = runTest("AbstractTypes8") 
  @Test def testAbstractTypes9 = runTest("AbstractTypes9") 

}

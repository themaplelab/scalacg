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
    settings.bootclasspath.append("bin")
    settings.pluginOptions.value = List("callgraph:tca", "callgraph:this")
    val g = new nsc.Global(settings)
    println("==============================")
    println(filename)
    println("==============================")
    try {
      new (g.Run).compile(List("junit/src/tests/" + filename + ".scala",
        "src/callgraph/annotation/target.scala",
        "src/callgraph/annotation/invocations.scala"))
    } catch {
      case ex: Throwable =>
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
  @Test def testTraits11 = runTest("Traits11")
  @Test def testTraits12 = runTest("Traits12")

  @Test def testThisType1 = runTest("ThisType1")
  @Test def testThisType2 = runTest("ThisType2")
  @Test def testThisType2b = runTest("ThisType2b")
  @Test def testThisType3 = runTest("ThisType3")

  @Test def testObjectInClass = runTest("ObjectInClass")
  @Test def testObjectInObject = runTest("ObjectInObject")
  
  @Test def testOverriding1 = runTest("Overriding1")
  @Test def testOverloading = runTest("Overloading")

  @Test def testSimple = runTest("Simple")

  @Test def testCaseClass1 = runTest("CaseClass1")
  @Test def testCaseClass2 = runTest("CaseClass2")
  @Test def testCaseClass3 = runTest("CaseClass3")

  @Test def testClosures1 = runTest("Closures1")
  @Test def testClosures2 = runTest("Closures2")

  @Test def testPathTypes1 = runTest("PathTypes1")
  @Test def testPathTypes2 = runTest("PathTypes2")
  @Test def testPathTypes3 = runTest("PathTypes3")
  @Test def testPathTypes4 = runTest("PathTypes4")
  @Test def testPathTypes5 = runTest("PathTypes5")
  @Test def testPathTypes6 = runTest("PathTypes6")
  @Test def testPathTypes7 = runTest("PathTypes7")

  @Test def testAbstractTypes1 = runTest("AbstractTypes1")
  @Test def testAbstractTypes2 = runTest("AbstractTypes2")
  @Test def testAbstractTypes3 = runTest("AbstractTypes3")
  @Test def testAbstractTypes4 = runTest("AbstractTypes4")
  @Test def testAbstractTypes5 = runTest("AbstractTypes5")
  @Test def testAbstractTypes6 = runTest("AbstractTypes6")
  @Test def testAbstractTypes7 = runTest("AbstractTypes7")
  @Test def testAbstractTypes8 = runTest("AbstractTypes8")
  @Test def testAbstractTypes9 = runTest("AbstractTypes9")
  @Test def testAbstractTypes9b = runTest("AbstractTypes9b")
  @Test def testAbstractTypes10 = runTest("AbstractTypes10")
  @Test def testAbstractTypes11 = runTest("AbstractTypes11")
  @Test def testAbstractTypes12 = runTest("AbstractTypes12")
  @Test def testAbstractTypes13 = runTest("AbstractTypes13")
  @Test def testAbstractTypes14 = runTest("AbstractTypes14")
  
  @Test def testSetterOverloading = runTest("SetterOverloading")

  @Test def testGenerics1 = runTest("Generics1")
  @Test def testGenerics2 = runTest("Generics2")
  @Test def testGenerics3 = runTest("Generics3")
  @Test def testGenerics4 = runTest("Generics4")
  @Test def testGenerics5 = runTest("Generics5")
  @Test def testGenerics6 = runTest("Generics6")
  @Test def testGenerics7 = runTest("Generics7")
  @Test def testGenerics8 = runTest("Generics8")
  @Test def testGenerics9 = runTest("Generics9")
  @Test def testGenerics10 = runTest("Generics10")
  @Test def testGenerics11 = runTest("Generics11")
  @Test def testGenerics12 = runTest("Generics12")
  @Test def testGenerics15 = runTest("Generics15")
  @Test def testGenerics16 = runTest("Generics16")

  @Test def testWeirdName1 = runTest("WeirdName1")
  @Test def testWeirdName2 = runTest("WeirdName2")

  @Test def testSuperCallSimple = runTest("SuperCallSimple")
  @Test def testSuperCallLibrary = runTest("SuperCallLibrary")
  @Test def testSuperCallTraitSimple = runTest("SuperCallTraitSimple")
  @Test def testSuperCallChoice = runTest("SuperCallChoice")
  @Test def testSuperCallQualified = runTest("SuperCallQualified")
  @Test def testSuperCall1 = runTest("SuperCall1")
  @Test def testSuperCall2 = runTest("SuperCall2")
  @Test def testSuperCall3 = runTest("SuperCall3")
  @Test def testSuperCall4 = runTest("SuperCall4")

  @Test def testImplicits1 = runTest("Implicits1")
  @Test def testImplicits2 = runTest("Implicits2")
  @Test def testImplicits3 = runTest("Implicits3")
  @Test def testImplicitArguments1 = runTest("ImplicitArguments1")
  @Test def testImplicitArguments2 = runTest("ImplicitArguments2")
  @Test def testImplicitArguments3 = runTest("ImplicitArguments3")

  @Test def testOperator1 = runTest("Operator1")

  @Test def testGetterMethod1 = runTest("GetterMethod1")
  @Test def testGetterMethod2 = runTest("GetterMethod2")
  @Test def testSealed1 = runTest("Sealed1")

  @Test def testBreakable = runTest("Breakable")
  @Test def testBreakable2 = runTest("Breakable2")

  @Test def testInfix = runTest("Infix")

  @Test def testMatch1 = runTest("Match1")
  @Test def testMatch2 = runTest("Match2")

  @Test def testReachable1 = runTest("Reachable1")
  @Test def testReachable2 = runTest("Reachable2")
  @Test def testReachable3 = runTest("Reachable3")
  
  @Test def testMultipleAnnotations = runTest("MultipleAnnotations")

  @Test def testExtractor = runTest("matching/Extractor")
  @Test def testExtractorVarargs = runTest("matching/ExtractorVarargs")
  @Test def testExtractorWithoutArgs = runTest("matching/ExtractorWithoutArgs")
  @Test def testCaseClass = runTest("matching/CaseClass")
  @Test def testCaseClassOption = runTest("matching/CaseClassOption")
  @Test def testCaseClassTuple = runTest("matching/CaseClassTuple")
  @Test def testCaseClassWildcard = runTest("matching/CaseClassWildcard")
  @Test def testMatchExpression = runTest("matching/MatchExpression")
  @Test def testExtractorUnapply = runTest("matching/ExtractorUnapply")
  @Test def testCaseClassInstanceof = runTest("matching/CaseClassInstanceof")
  @Test def testConstantEquals = runTest("matching/ConstantEquals")

  @Test def testToString1 = runTest("ToString1")
  
  @Test def testConstructor1 = runTest("Constructor1")
  
  @Test def testLiftedMethod1 = runTest("LiftedMethod1")
  
  @Test def testForeach2 = runTest("Foreach2")
  
  @Test def testLibraryCall = runTest("LibraryCall")
  @Test def testLibraryCall2 = runTest("LibraryCall2")
  
  @Test def testApplyConfusion1 = runTest("ApplyConfusion1")
  
  @Test def nested1 = runTest("Nested1")
  
  @Test def testGeneratedReflection1 = runTest("GeneratedReflection1")
  
  @Test def testMultipleBounds = runTest("MultipleBounds")
  
  @Test def testDefInDef = runTest("DefInDef")
}

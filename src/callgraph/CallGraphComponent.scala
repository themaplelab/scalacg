//package callgraph
//
//import java.io.PrintStream
//
//import scala.tools.nsc.Global
//import scala.tools.nsc.Phase
//import scala.tools.nsc.plugins.PluginComponent
//
//import callgraph.analysis.AbstractAnalysis
//import callgraph.analysis.RA
//import callgraph.analysis.TCA
//import callgraph.analysis.TCRA
//import callgraph.analysis.output.Assertions
//import callgraph.analysis.output.CGPrint
//import callgraph.analysis.output.Timer
//import callgraph.config.AnalysisOption
//import callgraph.config.Globals
//
///** Phase that resolves call sites to compute call graph */
//abstract class CallGraphGen extends PluginComponent {
//  val analysisOpt: AnalysisOption.Value
//  val globals: Globals
//  import globals._
//  import globals.global._
//
//  def newPhase(prevPhase: Phase) = {
//    Timer.start
//    println("Running " + analysisOpt.toString.toUpperCase)
//    
//    analysisOpt match {
//      case AnalysisOption.RaOption => new CallGraphPhase(prevPhase) with RA
//      case AnalysisOption.TcaOption => new CallGraphPhase(prevPhase) with TCA
//      case AnalysisOption.TdraOption => new CallGraphPhase(prevPhase) with TCRA
//    }
//  }
//
//  class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with Assertions with CGPrint {
//    this: AbstractAnalysis =>
//
//    val global: CallGraphGen.this.globals.global.type = CallGraphGen.this.globals.global
//    import global._
//
//    // apply is called for each file, but we want to run once for all files, that's why we override run later on.
//    def apply(unit: CompilationUnit) {
//      assert(assertion = false)
//    }
//
//    lazy val trees = currentRun.units.map(_.body).toList
//
//    override def run() {
//      initialize()
//      buildCallGraph()
//
//      println("It took: " + Timer.elapsed)
//
//      val methodstxt = new PrintStream("methods.txt")
//      printMethods(methodstxt)
//      methodstxt.close()
//
//      val callgraphgxl = new PrintStream("callgraph.gxl")
//      printProbeCallGraph(callgraphgxl)
//      callgraphgxl.close()
//
//      // Make sure any method annotated @reachable is in fact reachable in the call graph.
//      assertReachables(expectedReachables, reachableMethods)
//
//      // Make sure any method annotated @notreachable is in fact not reachable in the call graph.
//      assertNotReachables(expectedNotReachables, reachableMethods)
//
//      // Some more assertions
//      // TODO: we should have all these assertions in the Assertions trait and just call it there (code refactoring)
//      printAnnotatedCallsites()
//    }
//  }
//}
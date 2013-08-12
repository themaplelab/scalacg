package callgraph

import analysis.output.{Timer, CGAnnotations, Assertions, CGPrint}
import java.io.PrintStream
import scala.collection.immutable.List
import scala.collection.mutable
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import analysis.{TCRA, TCA, RA, AbstractAnalysis}

class CallGraphPlugin(val global: Global) extends Plugin { cgPlugin =>
  val name = "callgraph"
  val description = "builds a call graph"
  val components = List[PluginComponent](AnnotationComponent, CallGraphComponent)

  private val methodToId = mutable.Map[global.Symbol, Int]()
  private val expectedReachables = mutable.Set[global.Symbol]()
  private val expectedNotReachables = mutable.Set[global.Symbol]()
  private var _appClasses = Set[global.Type]() // had to use another name here to make the set of appClasses shareable across the two components

  object AnalysisOption extends Enumeration {
    type AnalysisOption = Value
    val RaOption = Value("ra")
    val TcaOption = Value("tca")
    val TdraOption = Value("tdra")
  }

  import AnalysisOption._

  // Plugin options
  var analysisOpt = TcaOption
  var doThis = false

  override def processOptions(options: List[String], error: String => Unit) {
    options match {
      case analysisName :: tail if AnalysisOption.values.map(_.toString) contains analysisName =>
        analysisOpt = AnalysisOption.withName(analysisName)
        processOptions(tail, error)
      case "this" :: tail =>
        doThis = true
        processOptions(tail, error)
      case option :: _ => error("Error, unknown option: " + option)
      case nil => // no more options to process
    }
  }

  /** Phase that resolves call sites to compute call graph */
  object CallGraphComponent extends PluginComponent { cgComponent =>
    val global: cgPlugin.global.type = cgPlugin.global
    val runsAfter = List[String]("targetannotation") // TODO: is this the right place for the phase?
    val phaseName = cgPlugin.name

    def newPhase(prevPhase: Phase) = {
      println("Running " + analysisOpt.toString.toUpperCase)
      Timer.start = System.currentTimeMillis()
      analysisOpt match {
        case RaOption => new CallGraphPhase(prevPhase) with RA
        case TcaOption => new CallGraphPhase(prevPhase) with TCA
        case TdraOption => new CallGraphPhase(prevPhase) with TCRA
      }
    }

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with Assertions with CGPrint {
      this: AbstractAnalysis =>

      // apply is called for each file, but we want to run once for all files, that's why we override run
      def apply(unit: cgComponent.global.CompilationUnit) {
        assert(assertion = false)
      }

      // The cake pattern stuff, you need to provide a "concrete" reference for Global
      val global = cgComponent.global
      import global._ // just saves you typing

      val methodToId = cgPlugin.methodToId

      lazy val trees = global.currentRun.units.map(_.body).toList
      lazy val appClasses = Set[Type](_appClasses.toSeq: _*) // initializing a set with elements from another set

      override def run() {
        initialize()
        buildCallGraph()

        // End the timer
        Timer.end = System.currentTimeMillis()
        println("It took: " + Timer.elapsed)

        val methodstxt = new PrintStream("methods.txt")
        printMethods(methodstxt)
        methodstxt.close()

        val callgraphgxl = new PrintStream("callgraph.gxl")
        printProbeCallGraph(callgraphgxl)
        callgraphgxl.close()

        //        val callgraphtxt = new PrintStream("callgraph.txt")
        //        printCallGraph(callgraphtxt)
        //        callgraphtxt.close()
        //
        //        val printcgtxt = new PrintStream("printcg.txt")
        //        printTextualCallGraph(printcgtxt)
        //        printcgtxt.close()
        //
        //        val reachablestxt = new PrintStream("reachables.txt")
        //        printReachableMethods(reachablestxt)
        //        reachablestxt.close()
        //
        //        val methodsOwnersTxt = new PrintStream("methodsOwners.txt")
        //        printMethodsOwners(methodsOwnersTxt)
        //        methodsOwnersTxt.close()
        //
        //        val eclipsecgtxt = new PrintStream("eclipsecg.txt")
        //        printEclipseCallGraph(eclipsecgtxt)
        //        eclipsecgtxt.close()

        // Make sure any method annotated @reachable is in fact reachable in the call graph.
        assertReachables(expectedReachables, reachableMethods)

        // Make sure any method annotated @notreachable is in fact not reachable in the call graph.
        assertNotReachables(expectedNotReachables, reachableMethods)

        // Some more assertions
        // TODO: we should have all these assertions in the Assertions trait and just call it there (code refactoring)
        printAnnotatedCallsites()
      }
    }
  }

  /** Phase that annotates each method with @callgraph.annotation.targetmethod(serial number) */
  private object AnnotationComponent extends PluginComponent { annotComponent =>
    val global: cgPlugin.global.type = cgPlugin.global
    val runsAfter = List[String]("uncurry")
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = "targetannotation"

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with CGAnnotations {
      val global = annotComponent.global
      import global._

      var serialNum = 1

      def apply(unit: annotComponent.global.CompilationUnit) {
        val valueName = newTermName("value")
        unit.body.foreach { node =>
          if (node.isInstanceOf[DefDef]) {
            // Add the serial number annotation 
            val annotationInfo = AnnotationInfo(targetmethodAnnotation.tpe, List(),
              List((valueName, LiteralAnnotArg(Constant(serialNum)))))
            node.symbol.addAnnotation(annotationInfo)
            methodToId += (node.symbol -> serialNum)
            serialNum += 1

            // Compile a list of methods that have @reachable annotation
            if (hasReachableAnnotation(node.symbol)) expectedReachables += node.symbol

            // Compile a list of methods that have @notreachable annotation
            if (hasNotReachableAnnotation(node.symbol)) expectedNotReachables += node.symbol
          } else if (node.isInstanceOf[ClassDef] || node.isInstanceOf[ModuleDef]) {
            _appClasses += node.symbol.tpe
          }
        }
      }
    }
  }
}

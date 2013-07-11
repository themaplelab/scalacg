package callgraph

import java.io.PrintStream
import scala.collection.immutable.List
import scala.collection.mutable
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import ca.uwaterloo.scalacg.util.Annotations
import ca.uwaterloo.scalacg.util.Assertions
import ca.uwaterloo.scalacg.util.Probe
import ca.uwaterloo.scalacg.util.Timer

class CallGraphPlugin(val global: Global) extends Plugin {
  val name = "callgraph"
  val description = "builds a call graph"
  val components = List[PluginComponent](AnnotationComponent, CallGraphComponent)
  val methodToId = mutable.Map[global.Symbol, Int]()
  var expectedReachables = Set[global.Symbol]()
  var expectedNotReachables = Set[global.Symbol]()
  var _appClasses = Set[global.Type]() // had to use another name here to make the set of appClasses shareable across the two components

  /** Phase that resolves call sites to compute call graph */
  private object CallGraphComponent extends PluginComponent {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
    val runsAfter = List[String]("targetannotation") // TODO: is this the right place for the phase?
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = CallGraphPlugin.this.name

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with CGUtils with Assertions with THA with Probe {
      // apply is called for each file, but we want to run once for all files, that's why we override run
      def apply(unit: CallGraphComponent.this.global.CompilationUnit) = assert(false)

      // The cake pattern stuff, you need to provide a "concrete" reference for Global
      val global = CallGraphComponent.this.global
      import global._ // just saves you typing

      val methodToId = CallGraphPlugin.this.methodToId

      var trees = List[Tree]() // global.Tree

      var appClasses = Set[Type]()

      override def run = {
        trees = global.currentRun.units.map(_.body).toList
        appClasses = Set[Type](_appClasses.toSeq: _*) // weird Scala syntax to initialize a set with elements from another set

        initialize
        buildCallGraph

        // End the timer
        Timer.end = System.currentTimeMillis()
        println("It took: " + Timer.elapsed)

        val callgraphtxt = new PrintStream("callgraph.txt")
        printCallGraph(callgraphtxt)
        callgraphtxt.close()

        val printcgtxt = new PrintStream("printcg.txt")
        printTextualCallGraph(printcgtxt)
        printcgtxt.close()

        val reachablestxt = new PrintStream("reachables.txt")
        printReachableMethods(reachablestxt)
        reachablestxt.close()

        val methodstxt = new PrintStream("methods.txt")
        printMethods(methodstxt)
        methodstxt.close()

        val methodsOwnersTxt = new PrintStream("methodsOwners.txt")
        printMethodsOnwers(methodsOwnersTxt)
        methodsOwnersTxt.close()

        val callgraphgxl = new PrintStream("callgraph.gxl")
        printProbeCallGraph(callgraphgxl)
        callgraphgxl.close()

        val eclipsecgtxt = new PrintStream("eclipsecg.txt")
        printEclipseCallGraph(eclipsecgtxt)
        eclipsecgtxt.close()

        // Make sure any method annotated @reachable is in fact reachable in the call graph.
        assertReachables(expectedReachables, reachableMethods)

        // Make sure any method annotated @notreachable is in fact not reachable in the call graph.
        assertNotReachables(expectedNotReachables, reachableMethods)

        // Some more assertions
        // TODO: we should have all these assertions in the Assertions trait and just call it there (code refactoring)
        printAnnotatedCallsites
      }
    }
  }

  /** Phase that annotates each method with @callgraph.annotation.targetmethod(serial number) */
  private object AnnotationComponent extends PluginComponent {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
    val runsAfter = List[String]("uncurry")
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = "targetannotation"

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with Annotations {
      val global = AnnotationComponent.this.global
      import global._

      var serialNum = 1

      def apply(unit: AnnotationComponent.this.global.CompilationUnit) = {
        // Start the timer
        Timer.start = System.currentTimeMillis()

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

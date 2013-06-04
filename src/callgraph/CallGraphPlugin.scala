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

class CallGraphPlugin(val global: Global) extends Plugin {
  val name = "callgraph"
  val description = "builds a call graph"
  val components = List[PluginComponent](AnnotationComponent, CallGraphComponent)
  val methodToId = mutable.Map[global.Symbol, Int]()
  var expectedReachables = Set[global.Symbol]()

  /** Phase that resolves call sites to compute call graph */
  private object CallGraphComponent extends PluginComponent {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
    val runsAfter = List[String]("refchecks") // TODO: is this the right place for the phase?
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = CallGraphPlugin.this.name

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with CGUtils with Assertions with THA {
      // apply is called for each file, but we want to run once for all files, that's why we override run
      def apply(unit: CallGraphComponent.this.global.CompilationUnit) = assert(false)

      // The cake pattern stuff, you need to provide a "concrete" reference for Global
      val global = CallGraphComponent.this.global
      import global._ // just saves you typing

      val methodToId = CallGraphPlugin.this.methodToId

      var trees = List[Tree]() // global.Tree

      override def run = {
        trees = global.currentRun.units.map(_.body).toList

        initialize
        buildCallGraph

        // Make sure any method annotated reachable is in fact reachable in the call graph.
        assertReachables(expectedReachables, reachableMethods)

        printAnnotatedCallsites

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
      }
    }
  }

  /** Phase that annotates each method with @callgraph.annotation.targetmethod(serial number) */
  private object AnnotationComponent extends PluginComponent {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
    val runsAfter = List[String]("parser")
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = "targetannotation"

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with Annotations {
      val global = AnnotationComponent.this.global
      import global._
      var serialNum = 1

      def apply(unit: AnnotationComponent.this.global.CompilationUnit) = {
        val valueName = newTermName("value")
        unit.body.foreach { node =>
          if (node.isInstanceOf[DefDef]) {
            // Add the serial number annotation 
            val annotationInfo = AnnotationInfo(targetmethodAnnotationTpe, List(),
              List((valueName, LiteralAnnotArg(Constant(serialNum)))))
            node.symbol.addAnnotation(annotationInfo)
            methodToId += (node.symbol -> serialNum)
            serialNum += 1

            // Compile a list of methods that have @reachable annotation
            if (hasReachableAnnotation(node.symbol)) expectedReachables += node.symbol
          }
        }
      }
    }
  }
}

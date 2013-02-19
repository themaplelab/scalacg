package callgraph

import scala.tools.nsc
import nsc.plugins.Plugin
import scala.collection.immutable.List
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.Phase
import sun.reflect.generics.tree.TypeSignature

class CallGraphPlugin(val global: Global) extends Plugin {
  val name = "callgraph"
  val description = "builds a call graph"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global = CallGraphPlugin.this.global
    val runsAfter = List[String]("refchecks") // TODO: is this the right place for the phase?
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = CallGraphPlugin.this.name

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with CGUtils with CHA {
      def apply(unit: Component.this.global.CompilationUnit) = assert(false)
      val global = Component.this.global
      import global._

      var trees = List[Tree]()

      override def run = {
        trees = global.currentRun.units.map(_.body).toList

        initialize
        buildCallGraph

        printAnnotatedCallsites

      }
    }
  }
}

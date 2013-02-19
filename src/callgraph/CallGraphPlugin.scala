package callgraph

import scala.tools.nsc
import nsc.plugins.Plugin
import scala.collection.immutable.List
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.Phase

class CallGraphPlugin(val global: Global) extends Plugin {
  val name = "callgraph"
  val description = "builds a call graph"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global = CallGraphPlugin.this.global
    import global._
    val runsAfter = List[String]("refchecks") // TODO: is this the right place for the phase?
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = CallGraphPlugin.this.name

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
      def apply(unit: global.CompilationUnit) = {
        for (tree <- unit.body) {
          tree match {
            case Apply(
              Select(
                Block(
                  List(annot),
                  receiver
                  ),
                methodName
                ), args) =>
              println("noticed call site with annotation: " + annot)

            case _ =>
          }
        }
      }
    }
  }
}
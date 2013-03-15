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
  val components = List[PluginComponent](AnnotationComponent, CallGraphComponent)

  /** Phase that resolves call sites to compute call graph */
  private object CallGraphComponent extends PluginComponent {
    val global = CallGraphPlugin.this.global
    val runsAfter = List[String]("refchecks") // TODO: is this the right place for the phase?
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = CallGraphPlugin.this.name

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) with CGUtils with THA {
      def apply(unit: CallGraphComponent.this.global.CompilationUnit) = assert(false)
      val global = CallGraphComponent.this.global
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

  /** Phase that annotates each method with @callgraph.annotation.targetmethod(serial number) */
  private object AnnotationComponent extends PluginComponent {
    val global = CallGraphPlugin.this.global
    import global._
    val runsAfter = List[String]("parser")
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = "targetannotation"

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
      val global = AnnotationComponent.this.global
      var serialNum = 1
      def apply(unit: AnnotationComponent.this.global.CompilationUnit) = {
        val targetAnnotationType =
          rootMirror.getRequiredClass("callgraph.annotation.targetmethod").info
        val valueName = newTermName("value")
        unit.body.foreach { node =>
          if (node.isInstanceOf[DefDef]) {
            val annotationInfo = AnnotationInfo(targetAnnotationType, List(),
              List((valueName, LiteralAnnotArg(Constant(serialNum)))))
            node.symbol.addAnnotation(annotationInfo)
            serialNum += 1
          }
        }
      }
    }
  }
}

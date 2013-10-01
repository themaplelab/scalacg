package callgraph

import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import callgraph.analysis.output.CGAnnotations
import callgraph.config.Globals
import scala.tools.nsc.Global
import callgraph.annotation.Annotations

/** Phase that annotates each method with @callgraph.annotation.MethodUID(serial number) */
abstract class MethodAnnotation extends PluginComponent {
  self: Annotations with Globals =>
  
  def newPhase(prevPhase: Phase) = new AnnotationPhase(prevPhase)

  class AnnotationPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
    var serialNumber = 1

    // We are overriding apply because we want this phase to run on each file separately.
    def apply(unit: global.CompilationUnit) {
      import global._
      unit.body.foreach { node =>
        if (node.isInstanceOf[DefDef]) {
          // Add the serial number annotation
          addMethodUID(node.symbol, serialNumber)
          methodToId += (node.symbol -> serialNumber)

          serialNumber += 1

        } else if (node.isInstanceOf[ClassDef] || node.isInstanceOf[ModuleDef]) {

        }
      }
    }
  }
}
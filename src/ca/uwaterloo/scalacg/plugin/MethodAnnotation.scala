package ca.uwaterloo.scalacg.plugin

import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent

import ca.uwaterloo.scalacg.util.Annotations
import ca.uwaterloo.scalacg.util.Timer

/**
 * Phase that annotates each method with @callgraph.annotation.MethodUID(serial number)
 */
abstract class MethodAnnotation extends PluginComponent with Annotations {

  def newPhase(prevPhase: Phase) = {
    new AnnotationPhase(prevPhase)
  }

  class AnnotationPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
    var serialNumber = 1

    override def run = {
      println(s"Starting phase $phaseName ...")
      Timer.start
      super.run
      println(s"Finished $phaseName in ${Timer.elapsed} seconds.")
      println
    }

    // We are overriding apply because we want this phase to run on each file separately.
    def apply(unit: global.CompilationUnit) = {
      import global._

      unit.body.foreach { node =>
        node match {
          case dd: DefDef => {
            val defdef = dd.symbol // NSC marks this as OPT, so just call it once

            // Add the serial number annotation
            addMethodUID(defdef, serialNumber)
            methodToId += (defdef -> serialNumber)
            serialNumber += 1

            // Compile a list of methods that have @reachable annotation
            if (hasReachableAnnotation(defdef)) {
              expectedReachables += defdef
            }

            // Compile a list of methods that have @notreachable annotation
            if (hasNotReachableAnnotation(defdef)) {
              expectedNotReachables += defdef
            }
          }
          case _ => // don't care
        }
      }
    }
  }
}
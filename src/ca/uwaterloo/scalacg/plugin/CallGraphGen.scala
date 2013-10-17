package ca.uwaterloo.scalacg.plugin

import scala.collection.immutable.{ Set => ImmutableSet }
import scala.collection.mutable.Map
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import ca.uwaterloo.scalacg.analysis.CallGraphAnalysis
import ca.uwaterloo.scalacg.util.Annotations
import ca.uwaterloo.scalacg.util.Assertions
import ca.uwaterloo.scalacg.util.Timer
import ca.uwaterloo.scalacg.util.CallGraphPrinter

/**
 * Phase that annotates generates the call graph.
 */
abstract class CallGraphGen extends PluginComponent {
  self: CallGraphAnalysis with Assertions with Annotations with CallGraphPrinter =>
  import global._

  // The call graph
  val callGraph = Map[CallSite, ImmutableSet[Symbol]]().withDefaultValue(ImmutableSet.empty[Symbol])

  // Plugin options
  val pluginOptions: PluginOptions

  def newPhase(prevPhase: Phase) = {
    println(s"Starting phase $phaseName ...")
    println("Running with the following options ...")
    println("Analysis: " + pluginOptions.analysis.toString.toUpperCase)
    println("Special handling for calls via \"super\": " + pluginOptions.doSuperCalls)
    println("Special handling for calls via \"this\": " + pluginOptions.doThis)
    println("Applying assertions: " + pluginOptions.doAssertions)

    Timer.start
    new CallGraphPhase(prevPhase)
  }

  class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
    // apply is called for each file, but we want to run once for all files, that's why we override run later on.
    def apply(unit: global.CompilationUnit) = {
      assert(assertion = false)
    }

    override def run = {
      initialize
      buildCallGraph

      println(s"Finished $phaseName in ${Timer.elapsed} seconds.")

      if (pluginOptions.doAssertions) {
        println("Performing assertions...")
        Timer.start
        doAssertions
        println(s"Finished assertions in ${Timer.elapsed} seconds.")
      }
      
      // Print out the probe call graph
      println("Printing call graph to disk...")
      printProbeCallGraph
      printMethods
    }
  }
}
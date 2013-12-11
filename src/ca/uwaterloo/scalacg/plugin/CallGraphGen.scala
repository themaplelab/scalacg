package ca.uwaterloo.scalacg.plugin

import scala.collection.immutable.{Set => ImmutableSet}
import scala.collection.mutable.Map
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent

import ca.uwaterloo.scalacg.analysis.CallGraphAnalysis
import ca.uwaterloo.scalacg.util.Annotations
import ca.uwaterloo.scalacg.util.Assertions
import ca.uwaterloo.scalacg.util.CallGraphPrinter
import ca.uwaterloo.scalacg.util.Timer

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
    new CallGraphPhase(prevPhase)
  }

  class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
    // apply is called for each file, but we want to run once for all files, that's why we override run later on.
    def apply(unit: global.CompilationUnit) = {
      assert(assertion = false)
    }

    private def printStats = {
      val abstractReceivers = collection.mutable.Set.empty[Type]
      // We should loop over abstract call sites in methods, other wise, 2 call sites from different methods are equal.
      callSitesInMethod.keys.foreach { m =>
        callSitesInMethod(m).foreach { cs =>
          callSitesTotalCount += 1
          if (!cs.isConstructorCall && !cs.isFunctionCall) {
            if (cs.isSuperCall) callSitesSuperCount += 1
            else {
              if (cs.hasThisReceiver) callSitesThisCount += 1
              if (cs.hasAbstractReceiver) callSitesAbstractTypesCount += 1
            }
          }
        }
      }

      println("Statistics")
      println("----------")
      println(s"# classes                   : $classesCount")
      println(s"# classes w abs type member : $classesAtmCount")
      println(s"# classes w abs type param  : $classesAtpCount")
      println(s"# anonfun                   : $anonfunCount")
      println(s"# objects                   : $objectsCount")
      println(s"# traits                    : $traitsCount")
      println(s"# trait compositions        : $traitCompsCount")
      println(s"# closures                  : $closuresCount")
      println(s"# methods                   : $methodsCount")
      println(s"# total call sites          : $callSitesTotalCount")
      println(s"# call sites on abs types   : $callSitesAbstractTypesCount")
      println(s"# call sites on this        : $callSitesThisCount")
      println(s"# call sites on super       : $callSitesSuperCount")
      println(s"# overriding methods        : $overridingMethodsCount")
      println
    }

    private def printOptions = {
      println(s"Starting phase $phaseName ...")
      println
      println("Running plugin with the following options ...")
      println(s"Analysis                : ${pluginOptions.analysis.toString.toUpperCase}")
      println(s"Handling super calls    : ${pluginOptions.doSuperCalls}")
      println(s"This special treatement : ${pluginOptions.doThis}")
      println(s"Applying assertions     : ${pluginOptions.doAssertions}")
      println
    }

    override def run = {
      printOptions

      Timer.start

      initialize
      buildCallGraph

      // Print the elapsed time of the analysis
      println
      println(s"Finished $phaseName in ${Timer.elapsed} seconds.")
      println

      printStats

      // Print out the probe call graph
      println("Printing call graph to disk ...")
      printProbeCallGraph
      printMethods
      printInstantiatedTypes
      printPackageNames

      if (pluginOptions.doAssertions) {
        println("Performing assertions ...")
        Timer.start
        doAssertions
        println(s"Finished assertions in ${Timer.elapsed} seconds.")
      }
    }
  }
}
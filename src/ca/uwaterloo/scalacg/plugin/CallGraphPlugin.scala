package ca.uwaterloo.scalacg.plugin

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent

import ca.uwaterloo.scalacg.analysis.Analysis
import ca.uwaterloo.scalacg.analysis.CallGraphAnalysis
import ca.uwaterloo.scalacg.config.GlobalConstants
import ca.uwaterloo.scalacg.util.Annotations
import ca.uwaterloo.scalacg.util.Assertions
import ca.uwaterloo.scalacg.util.CallGraphPrinter

class CallGraphPlugin(val global: Global) extends Plugin with GlobalConstants {
  import global._

  // Annotation-phase Collections
  val methodToId = Map[Symbol, Int]()
  val expectedReachables = Set[Symbol]()
  val expectedNotReachables = Set[Symbol]()

  // Plugin options
  val pluginOptions = PluginOptions()

  val name = CallGraphPluginName
  val description = CallGraphPluginDescription
  val components = List[PluginComponent](annotator, callgraphgen)

  // Process the plugin-specific options.
  override def processOptions(options: List[String], error: String => Unit) = {
    options match {
      case analysisName :: tail if Analysis.values.map(_.toString) contains analysisName => {
        pluginOptions.analysis = Analysis.withName(analysisName)
        processOptions(tail, error)
      }
      //      case "this" :: tail =>
      //        pluginOptions.doThis = true
      //        processOptions(tail, error)
      //      case "super" :: tail =>
      //        pluginOptions.doSuperCalls = true
      //        processOptions(tail, error)
      case "assert" :: tail => {
        pluginOptions.doAssertions = true
        processOptions(tail, error)
      }
      case option :: _ => error("Error, unknown option: " + option)
      case nil => // no more options to process
    }
  }

  // The annotation component
  lazy val annotator = new {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global

    val methodToId = CallGraphPlugin.this.methodToId
    val expectedReachables = CallGraphPlugin.this.expectedReachables
    val expectedNotReachables = CallGraphPlugin.this.expectedNotReachables

    /*
     * runsRightAfter ensures that the annotation phase will run "right after" the give phase. Otherwise,
     * the Scala compiler may inject intermediate phases before running our analysis. That can screw up
     * things, especially when constructs like anonymous classes is translated to something completely different
     * in those phases (namely: tailcalls, specialize).
     */
    override val runsRightAfter = Some[String](RunsAfterPhase)
    val runsAfter = List[String](RunsAfterPhase)
    val phaseName = AnnotationPhaseName
  } with MethodAnnotation

  // The call graph generation component
  lazy val callgraphgen = new {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
    val pluginOptions = CallGraphPlugin.this.pluginOptions

    val methodToId = CallGraphPlugin.this.methodToId
    val expectedReachables = CallGraphPlugin.this.expectedReachables
    val expectedNotReachables = CallGraphPlugin.this.expectedNotReachables

    override val runsRightAfter = Some[String](AnnotationPhaseName)
    val runsAfter = List[String](AnnotationPhaseName)
    val phaseName = CallGraphPluginName
  } with CallGraphGen with CallGraphAnalysis with Assertions with Annotations with CallGraphPrinter
}

/**
 * The plugin options.
 */
case class PluginOptions(var analysis: Analysis.Value = Analysis.Default,
  var doAssertions: Boolean = false) {

  lazy val doThis = analysis match {
    case Analysis.tca_expand_this => true
    case _ => false
  }

  lazy val doSuperCalls = analysis match {
    case Analysis.ra_all => false
    case _ => true
  }
}

package callgraph

import scala.collection.mutable.{ Map, Set }
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import callgraph.config.AnalysisOption
import callgraph.config.Phases
import callgraph.config.Globals
import callgraph.annotation.Annotations

class CallGraphPlugin(val global: Global) extends Plugin {
  import global._
  import Phases._
  
  val methodToId = Map[Symbol, Int]()
  // default value here to avoid an exception when methodToBody is called on Scala apply methods (have "null" bodies)
  // e.g. chessmaster had an exception when trying to retrieve the body for scala.AbstractFunction1.apply
  // Karim TODO: this shouldn't happen though since those apply methods are abstract and shouldn't be involved
  // in the method resolution process at all!
  val methodToBody = Map[Symbol, Tree]().withDefaultValue(EmptyTree)

  val expectedReachables = Set[Symbol]()
  val expectedNotReachables = Set[Symbol]()

  val appClasses = Set[Type]()

  val name = CallGraphPluginName
  val description = CallGraphPluginDescription
  val components = List[PluginComponent](annotator)//, callgraphgen)

  // Plugin options
  var analysisOpt = AnalysisOption.DefaultOption
  var doThis = false

  // Process the plugin-specific options, mainly these two: doThis, doTca.
  override def processOptions(options: List[String], error: String => Unit) {
    options match {
      case analysisName :: tail if AnalysisOption.values.map(_.toString) contains analysisName =>
        analysisOpt = AnalysisOption.withName(analysisName)
        processOptions(tail, error)
      case "this" :: tail =>
        doThis = true
        processOptions(tail, error)
      case option :: _ => error("Error, unknown option: " + option)
      case nil => // no more options to process
    }
  }

  /** Annotate every input method */
  lazy val annotator = new {
    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
    
    val methodToId = CallGraphPlugin.this.methodToId
    val methodToBody = CallGraphPlugin.this.methodToBody

    val expectedReachables = CallGraphPlugin.this.expectedReachables
    val expectedNotReachables = CallGraphPlugin.this.expectedNotReachables

    val appClasses = CallGraphPlugin.this.appClasses

    /*
     * runsRightAfter ensures that the annotation phase will run "right after" the give phase. Otherwise,
     * the Scala compiler may inject intermediate phases before running our analysis. That can screw up
     * things, especially when constructs like anonymous classes is translated to something completely different
     * in those phases (namely: tailcalls, specialize).
     */
    override val runsRightAfter = Some[String](RunsAfterPhase)
    val runsAfter = List[String](RunsAfterPhase)
    val phaseName = AnnotationPhaseName
  } with MethodAnnotation with Annotations with Globals

  /** Generate the call graph */
//  lazy val callgraphgen = new {
//    val global: CallGraphPlugin.this.global.type = CallGraphPlugin.this.global
//    val analysisOpt = CallGraphPlugin.this.analysisOpt
//
//    override val runsRightAfter = Some[String](Properties.AnnotationPhaseName)
//    val runsAfter = List[String](Properties.AnnotationPhaseName)
//    val phaseName = Properties.CallGraphPluginName
//  } with CallGraphGen
}

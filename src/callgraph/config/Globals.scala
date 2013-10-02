package callgraph.config

import scala.collection.mutable

/**
 * This is where all the global properties/finals shared across our code-base are found.
 */
object Phases {
  final val CallGraphPluginName = "callgraph"
  final val CallGraphPluginDescription = "builds a call graph"

  final val RunsAfterPhase = "uncurry" // The best place, so far, for our plugin to run after.

  final val AnnotationPhaseName = "methodannotation"
}

/**
 * The various analysis options offered by our plugin.
 */
object AnalysisOption extends Enumeration {
  type AnalysisOption = Value
  
  val RaOption = Value("ra") // Name-based analysis
  val TcaOption = Value("tca") // Trait-composition analysis
  val TdraOption = Value("tdra") // TODO: what's that?

  final val DefaultOption = TcaOption // The analysis by default runs TCA
}

/** All global maps, sets are defined here */
trait Globals extends Global {
  import global._
  
  val methodToId: mutable.Map[Symbol, Int]
  val methodToBody: mutable.Map[Symbol, Tree]
  
  val expectedReachables: mutable.Set[Symbol]
  val expectedNotReachables: mutable.Set[Symbol]
  
  val appClasses: mutable.Set[Type]
}

trait Global {
  val global: scala.tools.nsc.Global
}
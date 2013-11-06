package ca.uwaterloo.scalacg.config

import scala.collection.immutable.{Set => ImmutableSet}
import scala.collection.mutable.Map
import scala.collection.mutable.Set

import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.util.Worklist

/**
 * This is where all the global properties/finals shared across our code-base are found.
 */

trait Global {
  val global: scala.tools.nsc.Global
}

trait CallGraphCollections extends CallSites with Global {
  import global._

  val callGraph: Map[CallSite, ImmutableSet[Symbol]]
  val entryPoints: Set[Symbol]
  val callBacks: Set[Symbol]
  val moduleConstructors: Set[Symbol]
}

trait CallGraphWorklists extends CallGraphCollections {
  import global._

  val instantiatedTypes: Worklist[Type]
  val callSites: Worklist[AbstractCallSite]
  val reachableMethods: Worklist[Symbol]
  val superCalled: Worklist[Symbol]
}

trait GlobalConstants {
  lazy final val annotationPackage = "ca.uwaterloo.scalacg.annotation"
  lazy final val annotationPackageSrc = "src/" + annotationPackage.replaceAllLiterally(".", "/")

  final val CallGraphPluginName = "callgraph"
  final val CallGraphPluginDescription = "builds a call graph"

  final val RunsAfterPhase = "uncurry" // The best place, so far, for our plugin to run after.

  final val AnnotationPhaseName = "methodannotation"
}
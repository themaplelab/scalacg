package ca.uwaterloo.scalacg.util

import scala.tools.nsc.Global
import callgraph.CGUtils

trait Annotations {

  val global: Global
  import global._

  lazy val reachableAnnotation = rootMirror.getRequiredClass("callgraph.annotation.reachable")
  lazy val targetmethodAnnotation = rootMirror.getRequiredClass("callgraph.annotation.targetmethod")
  lazy val invocationsAnnotation = rootMirror.getRequiredClass("callgraph.annotation.invocations")
  lazy val targetAnnotation = rootMirror.getRequiredClass("callgraph.annotation.target")

  /**
   * Does a method have a @reachable annotation?
   */
  def hasReachableAnnotation(symbol: Symbol) = {
    val reach = symbol.annotations.collect {
      case AnnotationInfo(tpe, _, _) if tpe == reachableAnnotation.tpe => tpe
    }
    assert(reach.size <= 1) // There should only be a maximum of one @reachable annotation per method
    !reach.isEmpty
  }
  
}
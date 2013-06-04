package ca.uwaterloo.scalacg.util

import scala.tools.nsc.Global
import callgraph.CGUtils

trait Annotations {

  val global: Global
  import global._

  lazy val reachableAnnotationSym = rootMirror.getRequiredClass("callgraph.annotation.reachable")
  lazy val reachableAnnotationTpe = rootMirror.getRequiredClass("callgraph.annotation.reachable").tpe
  
  lazy val targetmethodAnnotationSym = rootMirror.getRequiredClass("callgraph.annotation.targetmethod")
  lazy val targetmethodAnnotationTpe = rootMirror.getRequiredClass("callgraph.annotation.targetmethod").tpe

  /**
   * Does a method have a @reachable annotation?
   */
  def hasReachableAnnotation(symbol: Symbol) = {
    val reach = symbol.annotations.collect {
      case AnnotationInfo(tpe, _, _) if tpe == reachableAnnotationTpe => tpe
    }
    assert(reach.size <= 1) // There should only be a maximum of one @reachable annotation per method
    !reach.isEmpty
  }
  
}
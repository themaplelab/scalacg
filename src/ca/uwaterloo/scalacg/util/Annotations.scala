package ca.uwaterloo.scalacg.util

import scala.tools.nsc.Global
import callgraph.CGUtils

trait Annotations {

  val global: Global
  import global._

  final lazy val reachableAnnotation = rootMirror.getRequiredClass("callgraph.annotation.reachable")
  final lazy val notReachableAnnotation = rootMirror.getRequiredClass("callgraph.annotation.notreachable")
  final lazy val targetmethodAnnotation = rootMirror.getRequiredClass("callgraph.annotation.targetmethod")
  final lazy val invocationsAnnotation = rootMirror.getRequiredClass("callgraph.annotation.invocations")
  final lazy val targetAnnotation = rootMirror.getRequiredClass("callgraph.annotation.target")
  final lazy val noInvocationsAnnotation = rootMirror.getRequiredClass("callgraph.annotation.noInvocations")
  final lazy val NONE = "__NONE__"

  /**
   * Does a method have a @reachable annotation?
   */
  def hasReachableAnnotation(symbol: Symbol) = hasAnnotation(symbol, reachableAnnotation)

  /**
   * Does a method have a @notreachable annotation?
   */
  def hasNotReachableAnnotation(symbol: Symbol) = hasAnnotation(symbol, notReachableAnnotation)

  def hasTargetMethodAnnotation(symbol: Symbol) = hasAnnotation(symbol, targetmethodAnnotation)

  def hasInvocationsAnnotation(symbol: Symbol) = hasAnnotation(symbol, invocationsAnnotation)

  def hasTargetAnnotation(symbol: Symbol) = hasAnnotation(symbol, targetAnnotation)

  def hasNoInvocationsAnnotation(symbol: Symbol) = hasAnnotation(symbol, noInvocationsAnnotation)

  private def hasAnnotation(symbol: Symbol, annotation: ClassSymbol) = {
    val annotType = annotation.tpe
    val reach = symbol.annotations.collect {
      case AnnotationInfo(tpe, _, _) if tpe == annotType => tpe
    }
    if (annotType == reachableAnnotation || annotType == notReachableAnnotation) {
      assert(reach.size <= 1) // There should only be a maximum of one @reachable or @notreachable annotation per method
    }
    reach.nonEmpty
  }
}
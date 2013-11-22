package ca.uwaterloo.scalacg.util

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.{ Set => ImmutableSet }

import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.config.Global
import ca.uwaterloo.scalacg.config.GlobalConstants

/**
 * That's where all annotation-related types, methods, operations should be handled
 */
trait Annotations extends Global with GlobalConstants with CallSites with Probe {
  import global._

  val methodToId: Map[Symbol, Int]
  val expectedReachables: Set[Symbol]
  val expectedNotReachables: Set[Symbol]

  final lazy val reachableAnnotation = rootMirror.getRequiredClass(annotationPackage + ".reachable")
  final lazy val notReachableAnnotation = rootMirror.getRequiredClass(annotationPackage + ".notreachable")
  final lazy val methodUIDAnnotation = rootMirror.getRequiredClass(annotationPackage + ".MethodUID")
  final lazy val invocationsAnnotation = rootMirror.getRequiredClass(annotationPackage + ".invocations")
  final lazy val targetAnnotation = rootMirror.getRequiredClass(annotationPackage + ".target")
  final lazy val noInvocationsAnnotation = rootMirror.getRequiredClass(annotationPackage + ".noInvocations")
  final lazy val NONE = "__NONE__"

  private val UNANNOT = "<unannotated>"

  /**
   * Make a new AnnotationInfo object with the given serial number as value.
   */
  def mkAnnotationInfo(serialNumber: Int) = {
    val valueName = newTermName("value")
    val annotationInfo = AnnotationInfo(methodUIDAnnotation.tpe, List.empty[Tree],
      List((valueName, LiteralAnnotArg(Constant(serialNumber)))))
    annotationInfo
  }

  /**
   * Add MethodUID annotation to a method.
   */
  def addMethodUID(symbol: Symbol, serialNumber: Int) = {
    assert(symbol.isMethod)
    symbol.addAnnotation(mkAnnotationInfo(serialNumber))
  }

  /**
   * Does a method have a @reachable annotation?
   */
  def hasReachableAnnotation(symbol: Symbol) = hasAnnotation(symbol, reachableAnnotation)

  /**
   * Does a method have a @notreachable annotation?
   */
  def hasNotReachableAnnotation(symbol: Symbol) = hasAnnotation(symbol, notReachableAnnotation)

  def hasMethodUIDAnnotation(symbol: Symbol) = hasAnnotation(symbol, methodUIDAnnotation)

  def hasInvocationsAnnotation(symbol: Symbol) = hasAnnotation(symbol, invocationsAnnotation)

  def hasNoInvocationsAnnotation(symbol: Symbol) = hasAnnotation(symbol, noInvocationsAnnotation)

  def hasTargetAnnotation(symbol: Symbol) = hasAnnotation(symbol, targetAnnotation)

  private def hasAnnotation(symbol: Symbol, annotation: ClassSymbol) = {
    val annotType = annotation.tpe

    if (symbol.hasAnnotation(annotation)) {
      // There should only be a maximum of one @reachable or @notreachable annotation per method
      if (annotType == reachableAnnotation || annotType == notReachableAnnotation) {
        assert((symbol.annotations filter (_ matches symbol)).size <= 1)
      }
      true
    } else {
      false
    }
  }

  /**
   * Get the @target annotation for the given method.
   */
  def findTargetAnnotations(symbol: Symbol) = {
    val targets = findStringAnnotations(symbol, targetAnnotation)
    targets.headOption.getOrElse(List(UNANNOT + " " + probeMethod(symbol).correctToString)).head
  }

  /**
   * Get the @invocations annotations for the the given symbol.
   */
  def findInvocationsAnnotations(symbol: Symbol) = {
    val targets = findStringAnnotations(symbol, invocationsAnnotation)
    targets.headOption.getOrElse(List.empty[String]).toSet
  }

  /**
   * Find an annotation that is represented by a list of strings (e.g., @target, @invocations).
   */
  private def findStringAnnotations(symbol: Symbol, annotation: ClassSymbol) = symbol.annotations.collect {
    case AnnotationInfo(tpe, args, _) if tpe == annotation.tpe =>
      args.map((arg) =>
        arg match {
          case Literal(Constant(string: String)) => string
        })
  }
}
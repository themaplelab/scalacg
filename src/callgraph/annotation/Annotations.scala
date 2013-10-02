package callgraph.annotation

import callgraph.config.Global

/** That's where all annotation-related types, methods, operations should be handled */
trait Annotations extends Global {
  import global._

  final lazy val reachableAnnotation = rootMirror.getRequiredClass("callgraph.annotation.reachable")
  final lazy val notReachableAnnotation = rootMirror.getRequiredClass("callgraph.annotation.notreachable")
  final lazy val methodUIDAnnotation = rootMirror.getRequiredClass("callgraph.annotation.MethodUID")
  final lazy val invocationsAnnotation = rootMirror.getRequiredClass("callgraph.annotation.invocations")
  final lazy val targetAnnotation = rootMirror.getRequiredClass("callgraph.annotation.target")
  final lazy val noInvocationsAnnotation = rootMirror.getRequiredClass("callgraph.annotation.noInvocations")
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

  def hasTargetMethodAnnotation(symbol: Symbol) = hasAnnotation(symbol, methodUIDAnnotation)

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
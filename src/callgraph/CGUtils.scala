package callgraph

import scala.tools.nsc

trait CGUtils {
  val global: nsc.Global
  import global._

  val UNANNOT = "<unannotated>"

  def trees: List[Tree]

  case class CallSite(receiver: Tree, method: MethodSymbol, args: List[Tree], annotation: List[String])

  var callSites = List[CallSite]()
  var classes = Set[ClassSymbol]()
  def callGraph: CallSite => Set[MethodSymbol]

  def initialize = {
    // find call sites
    trees.foreach { tree =>
      tree.foreach { node =>
        node match {
          case Apply(Select(receiver, methodName), args) =>
            // look for an annotation on the receiver
            val (annotation, plainReceiver) =
              receiver match {
                case Block(annotations, plainReceiver) =>
                  val annot = annotations.collect {
                    case Literal(Constant(string: String)) => string
                  }
                  (annot, plainReceiver)
                case _ => (List(), receiver)
              }
            callSites = CallSite(plainReceiver, node.symbol.asMethod, args, annotation) :: callSites
          case _ =>
        }
      }
    }

    // find classes
    classes = trees.flatMap { tree =>
      tree.collect { case cd: ClassDef => cd.symbol.asClass }
    }.toSet
  }

  // TODO: search for @target annotation; for now, just get first annotation
  def findTargetAnnotation(symbol: Symbol) = {
    val targetAnnotationType =
      rootMirror.getRequiredClass("tests.target").tpe
    val targets = symbol.annotations.collect {
      case AnnotationInfo(tpe, Literal(Constant(string: String)) :: _, _) if tpe == targetAnnotationType => string
    }
    assert(targets.size <= 1)
    targets.headOption.getOrElse(UNANNOT)
  }

  def lookup(receiverType: Type, staticTarget: MethodSymbol, consideredClasses: Set[ClassSymbol]): Set[MethodSymbol] = {
    var targets = List[MethodSymbol]()
    for {
      cls <- consideredClasses
      val tpe = cls.tpe
      if tpe <:< receiverType
      val target = tpe.member(staticTarget.name)
      if !target.isDeferred
    } {
      target match {
        case NoSymbol =>
          // TODO: can this ever happen? let's put in an assertion and see...
          assert(false)

        case _ =>
          // Disambiguate overloaded methods based on the types of the args
          if (target.isOverloaded) {
            targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)).map(_.asMethod) ::: targets
          } else {
            targets = target.asMethod :: targets
          }
      }
    }
    targets.toSet
  }

  def printAnnotatedCallsites = {
    for {
      callSite <- callSites
      if !callSite.annotation.isEmpty
    } {
      println(callSite)

      val resolved = callGraph(callSite).map(findTargetAnnotation)
      val expected = callSite.annotation.toSet
      println("Resolved: " + resolved.toSeq.sorted.mkString(", "))
      println("Expected: " + expected.toSeq.sorted.mkString(", "))
      assert(callSite.annotation.isEmpty || (resolved == expected), expected.toSeq.sorted.mkString(", "))
    }
  }
  
//  /**
//   * Map a name from bytecode to its original Scala name. This method is mainly used for call graph comparisons. 
//   */
//  def mapByteCodeName(byteCodeName : String) : Symbol {
//    
//  }
}

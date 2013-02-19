package callgraph

import scala.tools.nsc
import nsc.plugins.Plugin
import scala.collection.immutable.List
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.Phase

class CallGraphPlugin(val global: Global) extends Plugin {
  val name = "callgraph"
  val description = "builds a call graph"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global = CallGraphPlugin.this.global
    import global._
    val runsAfter = List[String]("refchecks") // TODO: is this the right place for the phase?
    def newPhase(prevPhase: Phase) = new CallGraphPhase(prevPhase)
    val phaseName = CallGraphPlugin.this.name

    class CallGraphPhase(prevPhase: Phase) extends StdPhase(prevPhase) {
      def apply(unit: global.CompilationUnit) = assert(false)
      override def run = {
        val trees = global.currentRun.units.map(_.body).toList
        val UNANNOT = "<unannotated>"

        case class CallSite(receiver: Tree, method: Name, args: List[Tree], annotation: List[String])
        def findCallSites = {
          var callSites = List[CallSite]()

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
                  callSites = CallSite(plainReceiver, methodName, args, annotation) :: callSites
                case _ =>
              }
            }
          }

          callSites
        }

        // TODO: search for @target annotation; for now, just get first annotation
        def findTargetAnnotation(symbol: Symbol) = {
          symbol.annotations match {
            case AnnotationInfo(tpe, Literal(Constant(string: String)) :: _, javaArgs) :: _ =>
              string
            case _ => UNANNOT
          }
        }

        //        case class Target(annotation: String, tree: DefDef)
        //        def findTargets: Set[Target] = {
        //          var targets = List[Target]()
        //
        //          trees.foreach { tree =>
        //            tree.foreach { node =>
        //              node match {
        //                case method @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        //                  // TODO: search for @target annotation; for now, just get first annotation
        //                  val annotation = method.symbol.annotations match {
        //                    case AnnotationInfo(tpe, Literal(Constant(string: String)) :: _, javaArgs) :: _ =>
        //                      string
        //                    case _ => UNANNOT
        //                  }
        //                  targets = Target(annotation, method) :: targets
        //              }
        //            }
        //          }
        //          targets.toSet
        //        }

        def findClasses: List[ClassDef] = {
          trees.flatMap { tree =>
            tree.collect { case cd: ClassDef => cd }
          }
        }
        val callSites = findCallSites
        //        val targets = findTargets
        val classes = findClasses

        def lookup(receiverType: Type, methodName: Name, args: List[Tree]): Set[Symbol] = {
          var targets = List[Symbol]()
          for {
            classDef <- classes
            val tpe = classDef.symbol.tpe
            if tpe <:< receiverType
            val target = tpe.member(methodName)
            if !target.isDeferred
          } {
            target match {
              case NoSymbol =>
                // TODO: can this ever happen? let's put in an assertion and see...
                assert(false)

              // TODO: use args to disambiguate overloaded methods
              case _ =>
                targets = target :: targets
            }
          }
          targets.toSet
        }

        for {
          callSite <- callSites
          if !callSite.annotation.isEmpty
        } {
          println(callSite)

          // resolved and resolved2 are equivalent. Which one do you find more readable?
          val resolved =
            for (target <- lookup(callSite.receiver.tpe, callSite.method, callSite.args))
              yield findTargetAnnotation(target)

          val resolved2 = lookup(callSite.receiver.tpe, callSite.method, callSite.args).
            map(findTargetAnnotation(_))

          val expected = callSite.annotation.toSet
          println("Resolved: " + resolved.toSeq.sorted.mkString(", "))
          println("Expected: " + expected.toSeq.sorted.mkString(", "))
          assert(callSite.annotation.isEmpty || (resolved == expected))
        }
      }
    }
  }
}
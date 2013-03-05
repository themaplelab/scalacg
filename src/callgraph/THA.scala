package callgraph

import scala.tools.nsc

trait THA { this: CGUtils =>
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[MethodSymbol]]()

  def buildCallGraph = {
    /* Given the ancestors of a This in the AST, determines the method that has that
     * particular This as its implicit this parameter.
     * 
     * It is possible that there is no such method. For example:
     * class A {
     *   val a = 5
     *   val b = this.a + 6
     * }
     * In such cases, returns NoSymbol
     */
    def containingMethod(ancestors: List[Tree], thisType: Symbol): Symbol = {
      (for {
        firstContainer <- ancestors.find{ node =>
          node.isInstanceOf[DefDef] || node.isInstanceOf[ClassDef]
        }
        instanceMethod <- firstContainer.symbol.ownersIterator.find { sym =>
          sym.isMethod && sym.owner == thisType
        }
      } yield instanceMethod).getOrElse(NoSymbol)
    }
    for (callSite <- callSites) {
      val targets = callSite.receiver match {
        case ths: This =>
          val method = containingMethod(callSite.ancestors, ths.symbol)
          if (method != NoSymbol)
            lookup(callSite.receiver.tpe, callSite.method, classes.filter(_.tpe.members.sorted.contains(method)))
          else
            lookup(callSite.receiver.tpe, callSite.method, classes)
        case _ =>
          lookup(callSite.receiver.tpe, callSite.method, classes)
      }
      callGraph += (callSite -> targets)
    }
  }
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

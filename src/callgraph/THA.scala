package callgraph

import scala.tools.nsc

trait THA extends CGUtils {
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[MethodSymbol]]()

  var superMethodNames = Set[TermName]()

  override def initialize = {
    super.initialize
    for {
      tree <- trees
      node <- tree
    } {
      node match {
        case Select(Super(_, _), name) => superMethodNames += name
        case _ =>
      }
    }
    addTypeConcretizations(classes)
  }
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
        firstContainer <- ancestors.find { node =>
          node.isInstanceOf[DefDef] || node.isInstanceOf[ClassDef]
        }
        instanceMethod <- firstContainer.symbol.ownersIterator.find { sym =>
          sym.isMethod && sym.owner == thisType
        }
      } yield instanceMethod).getOrElse(NoSymbol)
    }
    for (callSite <- callSites) {
      if (callSite.receiver == null) {
        callGraph += (callSite -> Set(callSite.method))
      } else {
        val thisSymbol =
          if (callSite.receiver.isInstanceOf[This]) callSite.receiver.symbol
          else if (callSite.receiver.tpe.isInstanceOf[ThisType])
            callSite.receiver.tpe.asInstanceOf[ThisType].sym
          else NoSymbol
        val filteredClasses =
          thisSymbol match {
            case NoSymbol => classes
            case symbol =>
              val method = containingMethod(callSite.ancestors, symbol)
              if (method == NoSymbol || superMethodNames.contains(method.name)) classes
              else
                classes.filter(_.tpe.members.sorted.contains(method))
          }
        val targets = lookup(callSite.receiver.tpe, callSite.method, filteredClasses)
        callGraph += (callSite -> targets)
      }
    }
  }
  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

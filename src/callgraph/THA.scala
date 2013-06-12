package callgraph

import scala.tools.nsc
import scala.collection.mutable

trait THA extends CGUtils {
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

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
  val classToMembers = mutable.Map[Symbol, Set[Symbol]]()

  var instantiatedClasses = Set[Symbol]()
  // in Scala, code can appear in classes as well as in methods, so reachableCode generalizes reachable methods
  var reachableCode = Set[Symbol]()

  // newly reachable methods to be processed
  val methodWorklist = mutable.Queue[Symbol]()

  def addMethod(method: Symbol) = {
    if (!reachableCode(method)) methodWorklist += method
  }

  // the set of classes instantiated in a given method
  lazy val classesInMethod = {
    val ret = mutable.Map[Symbol, Set[ClassSymbol]]().withDefaultValue(Set())
    def traverse(tree: Tree, owner: Symbol): Unit = {
      tree match {
        case _: ClassDef | _: DefDef =>
          tree.children.foreach(traverse(_, tree.symbol))
        case New(tpt) =>
          ret(owner) = ret(owner) + tpt.tpe.dealias.typeSymbol.asClass // some types are aliased, see CaseClass3
        case _ =>
          tree.children.foreach(traverse(_, owner))
      }
    }
    trees.foreach(traverse(_, NoSymbol))
    ret
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
     * 
     * TODO
     * Karim: For these cases, such a method is the primary constructor of the class.
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

    // all objects are considered to be allocated
    // Karim: Here isModuleOrModuleClass should be used instead of just isModule, or isModuleClass. I have no idea
    // why this works this way, but whenever I use either of them alone something crashes.
    instantiatedClasses ++= classes.filter(_.isModuleOrModuleClass)

    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    while (methodWorklist.nonEmpty) {
      // process new methods
      for (method <- methodWorklist.dequeueAll(_ => true)) {
        reachableCode += method
        instantiatedClasses ++= classesInMethod(method)
      }

      // process all call sites in reachable code
      for {
        callSite <- callSites
        if reachableCode contains callSite.enclMethod
      } {
        var targets = Set[Symbol]()

        if (callSite.receiver == null) {
          targets = Set(callSite.staticTarget)
        } else {
          val thisSymbol =
            if (callSite.receiver.isInstanceOf[This]) callSite.receiver.symbol
            else if (callSite.receiver.tpe.isInstanceOf[ThisType])
              callSite.receiver.tpe.asInstanceOf[ThisType].sym
            else NoSymbol
          val filteredClasses =
            thisSymbol match {
              case NoSymbol => instantiatedClasses
              case symbol =>
                val method = containingMethod(callSite.ancestors, symbol)
                if (method == NoSymbol || superMethodNames.contains(method.name)) instantiatedClasses
                else
                  instantiatedClasses.filter { cls =>
                    val members = classToMembers.getOrElseUpdate(cls, cls.tpe.members.sorted.toSet)
                    members.contains(method)
                  }
            }
          targets = lookup(callSite.receiver.tpe, callSite.staticTarget, filteredClasses)
        }

        callGraph += (callSite -> targets)
        targets.foreach(addMethod(_))
      }

      // add all constructors
      // TODO Karim: I don't understand how this adds class definition to reachable code? how is this later processed?
      instantiatedClasses.foreach(addMethod(_))
      for {
        cls <- instantiatedClasses
        constr <- cls.tpe.members
        if constr.isConstructor
      } {
        addMethod(constr)
      }

      // add the mixin primary constructors (see AbstractTypes13)
      //            for {
      //              cls <- instantiatedClasses
      //              mixin <- cls.mixinClasses
      //              val constr = mixin.primaryConstructor
      //              if constr != NoSymbol
      //            } {
      //              addMethod(constr)
      //            }
    }
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

package callgraph

import analysis.{TypeDependentAnalysis, WorklistAnalysis}
import scala.collection.mutable
import scala.collection.immutable.Set
import scala.Predef._

trait TCA extends WorklistAnalysis with TypeDependentAnalysis {

  import global._

  override def buildCallGraph() {

    var superCalls = Set[Symbol]()
    val classToMembers = mutable.Map[Type, Set[Symbol]]()
    var processedTypes = Set[Type]()

    // all objects are considered to be allocated
    // Karim: Here isModuleOrModuleClass should be used instead of just isModule, or isModuleClass. I have no idea
    // why this works this way, but whenever I use either of them alone something crashes.
    processedTypes ++= consideredTypes.filter(_.typeSymbol.isModuleOrModuleClass)
    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    while (methodWorklist.nonEmpty) {
      // Debugging info
      println("Items in work list: " + methodWorklist.size)

      processedTypes ++= processNewMethods(getClassesInMethod = true)
      superCalls ++= getNewSuperCalls(reachableCode)
      processCallSites(processedTypes, filterClasses = true, getFilteredClasses = getFilteredClasses)
      // TODO Karim: I don't understand how this adds class definition to reachable code? how is this later processed?
      addConstructorsToWorklist(processedTypes)
      addNewCallbacksToWorklist(processedTypes)
      // Type concretization now should happen inside the worklist too, and only for the instantiated classes
      // This should improve the precision of our analysis 
      addTypeConcretizations(processedTypes)
    }

    def getFilteredClasses(callSite: CallSite): Set[Type] = {
      val receiver = callSite.receiver
      val thisSymbol =
        if (receiver.isInstanceOf[This])
          receiver.symbol
        else if (receiver.tpe.isInstanceOf[ThisType])
          receiver.tpe.asInstanceOf[ThisType].sym
        else NoSymbol
      thisSymbol match {
        case NoSymbol => processedTypes
        case symbol =>
          val method = containingMethod(callSite.ancestors, symbol)
          if (method == NoSymbol || superCalls.contains(method))
            processedTypes
          else
            processedTypes.filter { tpe =>
              val members = classToMembers.getOrElseUpdate(tpe, tpe.members.sorted.toSet)
              members.contains(method)
            }
      }
    }

    def addTypeConcretizations(classes: Set[Type]) = {
      // find all definitions of abstract type members ("type aliases")
      for {
        tpe <- classes
        sym <- tpe.members // concrete type
        superClass <- tpe.baseClasses
        absSym <- superClass.tpe.decls // abstract type
        if absSym.isAbstractType
        if absSym.name == sym.name
      } {
        concretization +=
          (absSym -> (concretization.getOrElse(absSym, Set()) + sym.tpe.dealias))
      }

      // find all instantiations of generic type parameters (generics behave the same way)
      for {
        tpe <- classes
        cls = tpe.typeSymbol
      } {
        cls.info match {
          // class declaration and has a set of parents
          case ClassInfoType(parents, _, _) =>
            for { parent <- parents } {
              val args = parent.typeArguments
              val cstr = parent.typeConstructor
              val params = cstr.typeParams
              for {
                (arg, param) <- (args zip params)
              } {
                def paramToConcrete = param -> (concretization.getOrElse(param, Set() + arg))
                concretization += paramToConcrete
              }
            }
          case PolyType(typeParams, _) =>
            // handles the case: new List[Int]
            for {
              (arg, param) <- (tpe.typeArguments zip typeParams)
            } {
              concretization += (param -> (concretization.getOrElse(param, Set() + arg)))
            }
          case _ =>
          // TODO: are we missing any cases?
        }
      }

      // transitively follow abstract type concretizations
      var oldConcretization = concretization
      do {
        oldConcretization = concretization
        for {
          (absSym, tpes) <- concretization
          tpe <- tpes
        } {
          tpe match {
            case TypeRef(_, sym, _) =>
              concretization +=
                (absSym -> (concretization(absSym) ++ concretization.getOrElse(sym, Set())))
            case _ =>
          }
        }
      } while (oldConcretization != concretization)
    }

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
  }
}

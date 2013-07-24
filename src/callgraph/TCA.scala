package callgraph

import scala.collection.mutable
import scala.collection.immutable.Set
import scala.Predef._
import util.SuperCalls

trait TCA extends AbstractAnalysis with SuperCalls {

  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  private var concretization = Map[Symbol, Set[Type]]()

  def getClasses: Set[Type] = { //todo: instantiatedTypes
    trees.flatMap {
      tree =>
        tree.collect {
          case cd: ClassDef if cd.symbol.isModuleOrModuleClass => cd.symbol.tpe // isModuleClass -> an object, it gets auto instantiated
          case nw: New => nw.tpt.tpe // the set of all allocation sites
        }
    }.toSet
  }

  def buildCallGraph() {

    var superCalls = Set[Symbol]()
    val classToMembers = mutable.Map[Type, Set[Symbol]]()

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

    // all objects are considered to be allocated
    // Karim: Here isModuleOrModuleClass should be used instead of just isModule, or isModuleClass. I have no idea
    // why this works this way, but whenever I use either of them alone something crashes.
    instantiatedClasses ++= classes.filter(_.typeSymbol.isModuleOrModuleClass)

    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    while (methodWorklist.nonEmpty) {
      // Debugging info
      println("Items in work list: " + methodWorklist.size)

      // process new methods
      for (method <- methodWorklist.dequeueAll(_ => true)) {
        reachableCode += method
        instantiatedClasses ++= classesInMethod(method)
      }

      // find call sites that use super (e.g., super.foo())
      // Now this has been moved inside the worklist (see ThisType2)
      for {
        callSite <- callSites
        if isSuperCall(callSite)
        if reachableCode contains callSite.enclMethod
      } {
        superCalls += callSite.staticTarget
      }

      // process all call sites in reachable code
      for {
        callSite <- callSites
        if reachableCode contains callSite.enclMethod
      } {
        var targets = Set[Symbol]()

        val receiver = callSite.receiver
        val csStaticTarget: MethodSymbol = callSite.staticTarget
        if (receiver == null) {
          targets = Set(csStaticTarget)
        } else {
          val thisSymbol =
            if (receiver.isInstanceOf[This])
              receiver.symbol
            else if (receiver.tpe.isInstanceOf[ThisType])
              receiver.tpe.asInstanceOf[ThisType].sym
            else NoSymbol
          val filteredClasses: Set[Type] =
            thisSymbol match {
              case NoSymbol => instantiatedClasses
              case symbol =>
                val method = containingMethod(callSite.ancestors, symbol)
                if (method == NoSymbol || superCalls.contains(method))  // todo: don't understand why isSuperCall() doesn't work
                  instantiatedClasses
                else
                  instantiatedClasses.filter { tpe =>
                    val members = classToMembers.getOrElseUpdate(tpe, tpe.members.sorted.toSet)
                    members.contains(method)
                  }
            }
          val superSymbols = getSuperSymbols(callSite, instantiatedClasses)
          targets = lookup(csStaticTarget, filteredClasses, receiver.tpe) ++ superSymbols // todo: for super[T] lookup here not necessary
        }

        callGraph += (callSite -> targets)
        targets.foreach(addMethod)
      }

      // add all constructors
      // TODO Karim: I don't understand how this adds class definition to reachable code? how is this later processed?
      instantiatedClasses.map(_.typeSymbol).foreach(addMethod)
      for {
        cls <- instantiatedClasses
        constr <- cls.members
        if constr.isConstructor
      } {
        addMethod(constr)
      }

      // Library call backs are also reachable
      for {
        cls <- instantiatedClasses
        member <- cls.decls // loop over the declared members, "members" returns defined AND inherited members
        if isApplication(member) && isOverridingLibraryMethod(member)
      } {
        callbacks += member
      }
      callbacks.foreach(addMethod)

      // Type concretization now should happen inside the worklist too, and only for the instantiated classes
      // This should improve the precision of our analysis 
      addTypeConcretizations(instantiatedClasses)
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
                concretization +=
                  (param -> (concretization.getOrElse(param, Set() + arg)))
              }
            }
          case PolyType(typeParams, _) =>
            // handles the case: new List[Int]
            for {
              (arg, param) <- (tpe.typeArguments zip typeParams)
            } {
              concretization +=
                (param -> (concretization.getOrElse(param, Set() + arg)))
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
  }

  def lookup(staticTarget: MethodSymbol,
             consideredClasses: Set[Type],
             // default parameters, used only for super method lookup
             receiverType: Type,
             lookForSuperClasses: Boolean = false,
             getSuperName: (String => String) = (n: String) => n): Set[Symbol] = {

    def expand(t: Type): Set[Type] = {
      val sym = t.typeSymbol
      if (sym.isAbstractType) {
        concretization.getOrElse(sym, Set())
      } else {
        Set(t)
      }
    }

    def instantiateTypeParams(actual: Type, declared: Type): Type = {
      val tparams = declared.typeArgs
      // Using `actual` rather than `ThisType(actual.typeSymbol)`, the latter causes loss of generic type information
      // see (Generics4)
      val args = tparams map {
        _.asSeenFrom(actual, declared.typeSymbol)
      }
      declared.instantiateTypeParams(tparams map {
        _.typeSymbol
      }, args)
    }

    // If the target method is a constructor, no need to do the lookup.
    if (staticTarget.isConstructor)
      return Set(staticTarget)

    // If it's in the application, then resolve the call
    var targets = List[Symbol]()
    for {
      tpe <- consideredClasses
      expandedType <- expand(instantiateTypeParams(tpe, receiverType.widen))
      asf = expandedType.asSeenFrom(tpe, expandedType.typeSymbol)
      tpeasf = tpe.asSeenFrom(asf, tpe.typeSymbol)
      if tpeasf <:< asf || lookForSuperClasses
      target = if (lookForSuperClasses) {
        tpeasf.member(staticTarget.name.newName(getSuperName(staticTarget.name.toString))) // todo: bad bad bad
      } else tpeasf.member(staticTarget.name)
      if !target.isDeferred
    } {
      target match {
        case NoSymbol =>
          // TODO: can this ever happen? let's put in an assertion and see...
          assert(assertion = false, message = "tpe is " + tpe)

        case _ =>
          // Disambiguate overloaded methods based on the types of the args
          if (target.isOverloaded) {
            targets = target.alternatives.filter(_.tpe.matches(staticTarget.tpe)) ::: targets
          } else {
            targets = target :: targets
          }
      }
    }

    /*
     * If the static target is in the application, return the set of resolved targets.
     * Else, return the set of resolved targets in addition to the static target. The static target then stands for
     * all those edges that we couldn't compute because we do not analyze the library.
     */
    if (isLibrary(staticTarget)) { // todo: what for super methods?
      targets = staticTarget :: targets
    }

      targets.toSet
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

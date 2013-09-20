package callgraph.analysis

import scala.collection.mutable
import scala.collection.immutable.Set

trait TCA extends WorklistAnalysis with TypeDependentAnalysis {

  import global._

  override def buildCallGraph() {

    val superCalls = mutable.Set[Symbol]()
    val classToMembers = mutable.Map[Type, Set[Symbol]]()

    // All Scala objects are considered to be initially instantiated
    // Karim: Here isModuleOrModuleClass should be used instead of just isModule, or isModuleClass. I have no idea
    // why this works this way, but whenever I use either of them alone something crashes.
    val instantiatedTypes: mutable.Set[Type] = types.filter(_.typeSymbol.isModuleOrModuleClass)
    var newInstantiatedTypes: collection.Set[Type] = instantiatedTypes

    val reachableCallSites = mutable.Set[CallSite]()
    var newCallSites = collection.Set[CallSite]()
    var newReachableCode = mutable.Set[Symbol]()

    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    while (methodWorklist.nonEmpty) {
      // Debugging info
      println(s"Items in work list: ${methodWorklist.size}")

      // Process new types
      addConstructorsToWorklist(newInstantiatedTypes)
      addNewCallbacksToWorklist(newInstantiatedTypes)
      addTypeConcretizations(newInstantiatedTypes)

      // New reachable methods (this also updates reachableCode)
      newReachableCode = findNewReachableCode

      // New call sites
      newCallSites = findCallSites(newReachableCode)
      reachableCallSites ++= newCallSites

      // New instantiated types
      newInstantiatedTypes = findNewInstantiatedTypes(instantiatedTypes, newReachableCode)
      instantiatedTypes ++= newInstantiatedTypes

      // New super calls
      superCalls ++= findSuperCalls(newCallSites)

      // Process new call sites with all types, and use new types to process all call sites
      if (newCallSites.nonEmpty) {
        println(s"\tFound ${newCallSites.size} new call sites")
        processCallSites(newCallSites, instantiatedTypes, isTypeDependent = true, getFilteredClasses = getFilteredClasses)
      }
      if (newInstantiatedTypes.nonEmpty) {
        println(s"\tFound ${newInstantiatedTypes.size} new instantiated types")
        processCallSites(reachableCallSites, newInstantiatedTypes, isTypeDependent = true, getFilteredClasses = getFilteredClasses)
      }
    }

    def getFilteredClasses(callSite: CallSite, classes: collection.Set[Type]): collection.Set[Type] = {
      val thisEnclMethod = callSite.thisEnclMethod
      if(thisEnclMethod == NoSymbol || superCalls.contains(thisEnclMethod)) classes
      else classes.filter{
              tpe =>
                classToMembers.getOrElseUpdate(tpe, tpe.members.sorted.toSet).contains(thisEnclMethod)
      }
    }

    def addTypeConcretizations(types: collection.Set[Type]) = {
      // find all definitions of abstract type members ("type aliases")
      for {
        tpe <- types
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
        tpe <- types
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
  }
}

package ca.uwaterloo.scalacg.util

import scala.collection.mutable

import ca.uwaterloo.scalacg.config.Global

trait TypesCollections extends Global {
  import global._

  val applicationTypes: mutable.Set[Type]
  val types: mutable.Set[Type]
  val mainModules: mutable.Set[Type]
  val thisEnclMethodToTypes: mutable.Map[Symbol, Set[Type]]
  val packageNames: mutable.Set[String]
}

trait TypeOps extends TypesCollections {
  import global._

  def concretization: AbstractTypeConcretization

  /**
   * Get the constructors of a type.
   * Karim: we should be using decls here not members.
   */
  def constructorsOf(tpe: Type) = {
    //    tpe.members.filter(_.isConstructor).toSet
    tpe.decls.filter(_.isConstructor).toSet
  }

  /**
   * Is this symbol in an application class?
   */
  def isApplication(symbol: Symbol) = packageNames contains symbol.pkg
  //    applicationTypes contains symbol.owner.tpe
  //  (methodToId.get(symbol).isDefined) || (applicationTypes contains symbol.owner.tpe)

  /**
   * Is this symbol in a library class?
   */
  def isLibrary(symbol: Symbol) = !isApplication(symbol)

  /**
   * Return a pair of the expanded type as seen from tpe, and tpe as seen from expanded type.
   */
  def asSeenFrom(expanded: Type, tpe: Type) = {
    val expandedAsSeenFromTpe = expanded.asSeenFrom(tpe, expanded.typeSymbol)
    val tpeAsSeenFromExpanded = tpe.asSeenFrom(expandedAsSeenFromTpe, tpe.typeSymbol)
    (expandedAsSeenFromTpe, tpeAsSeenFromExpanded)
  }

  /**
   * Replace formal type parameter symbols with actual type arguments.
   */
  def instantiateTypeParams(actual: Type, declared: Type): Type = {
    val params = declared.typeArgs.map(_.typeSymbol)
    val args = actual.typeArgs
    if (params.length != args.length) declared else {
      val ret = declared.instantiateTypeParams(params, args)
      if (ret.isError) declared else ret
    }
  }

  /**
   * For those types that contain tpe in their linearizations, get those linearizations,
   * starting from the given type (wherever its linearization order is).
   */
  def trimmedLinearizations(types: mutable.Set[Type], cls: Symbol) = {
    val trimmed = mutable.Set[List[Symbol]]()
    types.foreach { tpe => if (tpe.baseClasses contains cls) trimmed += (tpe.baseClasses dropWhile (_ != cls)).tail }
    trimmed
  }

  /**
   * Get a string representation for the linearization of tpe.
   */
  def lineariztionStringOf(tpe: Type, sep: String = "\t") = {
    tpe.baseClasses.map(_.fullName).mkString(sep)
  }

  trait AbstractTypeConcretization {
    val global: TypeOps.this.global.type = TypeOps.this.global
    /**
     * Expand the abstract type to its concrete types.
     */
    def expand(t: Type): Set[Type]
    /**
     * Compute the type concretization for the given type
     */
    def addTypeConcretization(tpe: Type): Unit = {}
    /**
     * Compute the type concretization for the given method call
     */
    def addMethodInstantiation(method: Symbol, args: List[Type]): Unit = {}
    
    protected def upperBound(tpe: Type): Type = {
      val bound = tpe.bounds.hi
      assert(bound != NoType) // NOTE: this should never happen
      if (bound.typeSymbol.isAbstractType || bound.typeArguments.exists(_.typeSymbol.isAbstractType))
        definitions.AnyTpe
      else
        bound
    }
  }

  class BoundsTypeConcretization extends AbstractTypeConcretization {
    import global._

    def expand(t: Type) = {
      val sym = t.typeSymbol
      if (sym.isAbstractType) {
        Set(upperBound(t))
      } else {
        Set(t)
      }
    }
  }

  class TypeConcretization extends AbstractTypeConcretization {
    import global._

    val concretization = mutable.Map[Symbol, Set[Type]]().withDefaultValue(Set())

    val maxConcretizations = 1000

    var needsClosure = false

    def expand(t: Type) = {
      if (needsClosure) computeClosure
      val sym = t.typeSymbol
      if (sym.isAbstractType) {
        val conc = concretization(sym)
        if (conc.contains(NoType))
          // NoType indicates falling back to bounds-based analysis
          Set(upperBound(t))
        else
          conc
      } else {
        Set(t)
      }
    }

    private def addConcreteType(absSym: Symbol, newTpe: Type): Unit = {
      val old = concretization(absSym)
      if (!old.contains(NoType))
        concretization += absSym -> (old + newTpe)
    }
    private def addConcreteTypes(absSym: Symbol, newTpe: Set[Type]): Unit = {
      val old = concretization(absSym)
      if (!old.contains(NoType))
        concretization += absSym -> (old ++ newTpe)
    }
    /**
     * TODO Karim: this method definitely needs some optimization.
     */
    override def addTypeConcretization(tpe: Type) = {
      // Find all definitions of abstract type members ("type aliases")
      for {
        sym <- tpe.members // concrete type
        superClass <- tpe.baseClasses
        absSym <- superClass.tpe.decls // abstract type
        if absSym.isAbstractType
        if absSym.name == sym.name
      } {
        addConcreteType(absSym, sym.tpe.dealias)
      }

      // Find all instantiations of generic type parameters (generics behave the same way)
      tpe.typeSymbol.info match {
        // class declaration and has a set of parents
        case ClassInfoType(parents, _, _) =>
          for { parent <- parents } {
            val args = parent.typeArguments
            val cstr = parent.typeConstructor
            val params = cstr.typeParams
            for {
              (arg, param) <- (args zip params)
            } {
              addConcreteType(param, arg)
            }
          }
        case PolyType(typeParams, _) =>
          // handles the case: new List[Int]
          for {
            (arg, param) <- (tpe.typeArguments zip typeParams)
          } {
            addConcreteType(param, arg)
          }
        case _ =>
        // TODO: are we missing any cases?
      }
      needsClosure = true
    }

    private def computeClosure(): Unit = {
      // transitively follow abstract type concretizations
      var changed = false
      do {
        changed = false
        for {
          (absSym, tpes) <- concretization
          tpe <- tpes
        } {
          var newConc = concretization(absSym)
          tpe match {
            case TypeRef(_, sym, _) =>
              if (concretization(sym).contains(NoType))
                // NoType indicates falling back to bounds-based analysis
                newConc = Set(NoType)
              else
                newConc ++= concretization(sym)
            case _ =>
          }
          val origParams = tpe.typeArgs.map(_.typeSymbol)
          val params = origParams.map(_.deSkolemize)
          if (!params.isEmpty) {
            def crossProduct[A](list: List[Set[A]]): Set[List[A]] = list match {
              case Nil => Set(List())
              case x :: xs =>
                val tails = crossProduct(xs)
                for (chosen_x <- x; tail <- tails)
                  yield chosen_x :: tail
            }
            val instantiations = params.map(concretization)
            if (instantiations.exists(_.contains(NoType))) {
              // NoType indicates falling back to bounds-based analysis
              newConc = Set(NoType)
            } else {
              val argSet = crossProduct(params.map(concretization))
              val instantiatedTypes =
                argSet.map(tpe.instantiateTypeParams(origParams, _))
              newConc ++= instantiatedTypes
            }
          }
          if (newConc.size > maxConcretizations) {
            // NoType indicates falling back to bounds-based analysis
            newConc = Set(NoType)
            println("WARNING: abstract type " + absSym + " has more than " + maxConcretizations + " concretizations; falling back to bounds-based")
          }
          if (newConc != concretization(absSym)) {
            concretization += (absSym -> newConc)
            changed = true
          }
        }
      } while (changed)
      needsClosure = false
    }

    override def addMethodInstantiation(method: Symbol, args: List[Type]) = {
      assert(method.tpe.typeParams.length == args.length)
      for ((param, arg) <- method.tpe.typeParams zip args) {
        addConcreteType(param, arg)
      }
      needsClosure = true
    }
  }
}
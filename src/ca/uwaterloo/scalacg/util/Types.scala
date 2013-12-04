package ca.uwaterloo.scalacg.util

import scala.collection.immutable.{ Set => ImmutableSet }
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import ca.uwaterloo.scalacg.config.Global
import ca.uwaterloo.scalacg.plugin.PluginOptions
import ca.uwaterloo.scalacg.analysis.Analysis.Ba
import ca.uwaterloo.scalacg.util.Worklist
import scala.collection.immutable.{ Set => ImmutableSet }

trait TypesCollections extends Global {
  import global._

  val applicationTypes: Set[Type]
  val types: Set[Type]
  val mainModules: Set[Type]
  val thisEnclMethodToTypes: Map[Symbol, ImmutableSet[Type]]
  val packageNames: Set[String]

  class TypeWorklist[A <: Type] extends Worklist[A] {
    override def +=(elem: A) = {
      // No new types are accepted if they equivalent to types that have been reachable before
      if (!reachableItems.exists(e => e =:= elem)) {
        newItems += elem
        reachableItems += elem
        //      if(elem.toString contains "ConcreteType") println("adding ConcreteType to set of reachables :: " + elem.getClass)
      }
    }
  }
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
  def trimmedLinearizations(types: Set[Type], cls: Symbol) = {
    val trimmed = Set[List[Symbol]]()
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
  }

  class BoundsTypeConcretization extends AbstractTypeConcretization {
    import global._

    def expand(t: Type) = {
      val sym = t.typeSymbol
      if (sym.isAbstractType) {
        val upperBound = t.bounds.hi
        assert(upperBound != NoType) // NOTE: this should never happen
        //        assert(!upperBound.typeSymbol.isAbstractType) // NOTE: Karim: occurs in the benchmark factorie
        if (upperBound.typeSymbol.isAbstractType || upperBound.typeArguments.exists(_.typeSymbol.isAbstractType)) {
          Set(definitions.AnyTpe)
        } else {
          Set(upperBound)
        }
      } else {
        Set(t)
      }
    }
  }

  class TypeConcretization extends AbstractTypeConcretization {
    import global._

    val concretization = Map[Symbol, Set[Type]]()

    def expand(t: Type) = {
      val sym = t.typeSymbol
      if (sym.isAbstractType) {
        concretization.getOrElse(sym, Set())
      } else {
        Set(t)
      }
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
        concretization +=
          (absSym -> (concretization.getOrElse(absSym, Set()) + sym.tpe.dealias))
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
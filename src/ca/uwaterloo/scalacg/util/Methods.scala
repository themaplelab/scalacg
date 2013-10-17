package ca.uwaterloo.scalacg.util

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import ca.uwaterloo.scalacg.config.Global

trait MethodsCollections extends Global {
  import global._

  val methodToBody: Map[Symbol, Tree]
  val mainMethods: Set[Symbol]
  val mainModulesPrimaryConstructors: Set[Symbol]
}

trait MethodOps extends TypeOps {
  import global._

  /**
   * Does a symbol override a library method?
   */
  def isOverridingLibraryMethod(symbol: Symbol) = {
    !symbol.isConstructor && symbol.isMethod && !symbol.isDeferred &&
      symbol.isOverridingSymbol && symbol.allOverriddenSymbols.filter(isLibrary).nonEmpty
  }

  /**
   * Get the potential callback methods of a type.
   */
  def callBacksOf(tpe: Type) = {
    tpe.decls.filter(d => isApplication(d) && isOverridingLibraryMethod(d)).toSet
  }
}
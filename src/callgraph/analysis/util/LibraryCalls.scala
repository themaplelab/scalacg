package callgraph.analysis.util

import ca.uwaterloo.scalacg.util.Probe
import callgraph.analysis.{TreeTraversal, AbstractAnalysis}

trait LibraryCalls {

  this: TreeTraversal with AbstractAnalysis with Probe =>

  import global._

  var appClasses: Set[Type]

  /**
   * Is this symbol in an application class?
   */
  def isApplication(symbol: Symbol) = appClasses contains symbol.enclClass.tpe

  /**
   * Is this symbol in a library class?
   */
  def isLibrary(symbol: Symbol) = !isApplication(symbol)

  /**
   * Does a symbol override a library method?
   */
  def isOverridingLibraryMethod(symbol: Symbol) =
    !symbol.isConstructor && symbol.isMethod && !symbol.isDeferred &&
      symbol.isOverridingSymbol && symbol.allOverriddenSymbols.filter(isLibrary).nonEmpty

  /**
   * Get all the library methods overridden by this method. The methods are sorted according to the reverse
   * linearization.
   */
  def libraryOverriddenMethods(symbol: Symbol) = {
    var result = List[Symbol]()
    if (symbol.isMethod && symbol.allOverriddenSymbols.nonEmpty) {
      result = symbol.allOverriddenSymbols.filter(sym => isLibrary(sym))
    }
    result
  }

  /**
   * Get the topmost library method overridden by the this method. If the method is in the library and doesn't
   * override any method, then it is its own topmostLibraryOverriddenMethod
   */
  def topmostLibraryOverriddenMethod(symbol: Symbol) = {
    val methods = libraryOverriddenMethods(symbol)
    if (methods.nonEmpty) (methods filter (!_.isDeferred)).head
    else if (isLibrary(symbol)) symbol
    else NoSymbol
  }
}

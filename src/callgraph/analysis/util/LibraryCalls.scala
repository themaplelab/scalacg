package callgraph.analysis.util

import callgraph.analysis.{TreeTraversal, AbstractAnalysis}
import callgraph.analysis.output.Probe

trait LibraryCalls {

  this: TreeTraversal with AbstractAnalysis with Probe =>

  import global._

  var appClasses: Set[Type]

  private var cacheFoundOverridingLibraryMethods = Set[Symbol]()
  private var cacheMethodToLibraryOverriddenMethods = Map[Symbol, List[Symbol]]()      // todo: is that redundant?

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
  def isOverridingLibraryMethod(symbol: Symbol): Boolean = {
    if (cacheFoundOverridingLibraryMethods contains symbol)
      return true
    if (!symbol.isConstructor && symbol.isMethod && !symbol.isDeferred &&
      symbol.isOverridingSymbol && symbol.allOverriddenSymbols.filter(isLibrary).nonEmpty) {
      cacheFoundOverridingLibraryMethods += symbol
      return true
    }
    false
  }

  /**
   * Get all the library methods overridden by this method. The methods are sorted according to the reverse
   * linearization.
   */
  private def libraryOverriddenMethods(symbol: Symbol): List[Symbol] = {
    if (cacheMethodToLibraryOverriddenMethods contains symbol)
      return cacheMethodToLibraryOverriddenMethods(symbol)
    var result = List[Symbol]()
    if (symbol.isMethod && symbol.allOverriddenSymbols.nonEmpty) {
      result = symbol.allOverriddenSymbols.filter(sym => isLibrary(sym))
    }
    cacheMethodToLibraryOverriddenMethods += (symbol -> result)
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

package callgraph.analysis.output

import probe.ObjectManager
import probe.ProbeMethod
import callgraph.analysis.util.Global

trait Probe extends Global {

  import global._
  
  class StringHelper(str: String) {
    def replaceLast(regex: String, replacement: String) = str.reverse.replaceFirst(regex, replacement.reverse).reverse
  }
  implicit def stringWrapper(string: String) = new StringHelper(string)

  /**
   * A printable name for a that uses the probe signature, surround by "<" and ">", useful when printing sets of methods.
   */
  def signature(method: Symbol) = {
    "<" + probeMethod(method) + ">"
  }

  /**
   * Get a probe method for the given symbol
   */
  def probeMethod(methodSymbol: Symbol): ProbeMethod = {
    val probeClass = ObjectManager.v().getClass(effectiveOwnerName(methodSymbol))
    val probeMethod = ObjectManager.v().getMethod(probeClass, methodSymbol.simpleName.decode, paramsSignatureString(methodSymbol))
    probeMethod
  }

  /**
   * Get the full name (dot separated) of the owner of a method symbol. That acts like the method declaring class in Soot.
   */
  def effectiveOwnerName(methodSymbol: Symbol): String = {
    methodSymbol.fullName.replace("." + methodSymbol.simpleName, "")
  }

  /**
   * Get the params string of a method symbol
   */
  def paramsSignatureString(methodSymbol: Symbol): String = {
    methodSymbol.signatureString.substring(0, methodSymbol.signatureString.indexOf(')') + 1).replace("(", "").replace(")", "")
  }

  /**
   * Return zero if x is negative.
   */
  def getOrZero(x: Int) = {
    x match {
      case n if n < 0 => 0
      case n => n
    }
  }
}


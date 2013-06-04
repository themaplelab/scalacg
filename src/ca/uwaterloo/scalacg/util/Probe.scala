package ca.uwaterloo.scalacg.util

import probe.ObjectManager
import probe.ProbeMethod
import scala.tools.nsc.Global

trait Probe {
  
  val global: Global
  import global._

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

}
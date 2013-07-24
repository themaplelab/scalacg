package callgraph.analysis.util

import ca.uwaterloo.scalacg.util.Probe
import tools.nsc

object Signature extends Probe {

  val global: nsc.Global = null

  import global._
  import global.definitions._

  /**
   * Return a Soot-like method signature.
   */
  def methodSignature(methodSymbol: Symbol): String = {
    "<" + methodSymbol.fullName + methodSymbol.signatureString + ">"
  }

  /**
   * Get the bytecode descriptor for the given symbol
   */
  def bytecodeSignature(methodSymbol: Symbol): String = {
    var bf = ""

    //    bf += methodSymbol.effectiveOwner.javaClassName
    //    bf += ": " + methodSymbol.simpleName.encode
    //    bf += " ("
    for {
      param <- methodSymbol.tpe.params
    } {
      bf += signature(param.tpe)
    }
    //    bf += ")"
    bf
  }

  /**
   * Get the bytecode signature of a type
   */
  def signature(tpe: Type): String = {
    tpe.erasure match {
      case BooleanTpe => "Z"
      case ByteTpe => "B"
      case CharTpe => "C"
      case DoubleTpe => "D"
      case FloatTpe => "F"
      case IntTpe => "I"
      case LongTpe => "L"
      case ShortTpe => "S"
      case TypeRef(_, ArrayClass, arg :: Nil) => ("[") + signature(arg)
      case UnitTpe => "V"
      case _ if tpe.erasure <:< AnyRefTpe => "L" + tpe.erasure.typeSymbol.javaBinaryName + ";"
      case _ => assert(assertion = false, message = "unknown type: " + tpe.erasure); null
    }
  }
}

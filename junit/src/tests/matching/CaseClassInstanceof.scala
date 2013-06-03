package tests.matching

import tests.target
import tests.invocations

object CaseClassInstanceof {

   /**
    * Testing extractors
    */
   @invocations("<unannotated> scala.Any: isInstanceOf()")
   def main(args: Array[String]) {
    val e: Expr = Lit(value = true)
    e match {
      case Lit(v)    => println("right")
      case _         => println("wrong")
    }
  }
 }

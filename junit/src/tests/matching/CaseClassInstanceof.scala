package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

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

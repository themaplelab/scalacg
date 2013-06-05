package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object ExtractorUnapply {

   /**
    * Testing extractors
    */
   @invocations("<unannotated> tests.matching.ExtractorUnapply.Twice: apply(x: Int)", 
                "<unannotated> tests.matching.ExtractorUnapply.Twice: unapply(z: Int)")
   def main(args: Array[String]) {
     val x = Twice(21)
     x match {
       case Twice(y) => Console.println("right")
       case _        => Console.println("wrong")
     }
   }

   object Twice {
    def apply(x:Int) = x * 2
    def unapply(z:Int) = if (z % 2 == 0) Some(z/2) else None
  }
 }

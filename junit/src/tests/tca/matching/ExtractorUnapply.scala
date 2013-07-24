package tests.tca.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object ExtractorUnapply {

   /**
    * Testing extractors
    */
   @invocations("13: apply", "15: unapply")
   def main(args: Array[String]) {
     val x = Twice(21)
     x match {
       case Twice(y) => Console.println("right")
       case _        => Console.println("wrong")
     }
   }

   object Twice {
    @target("apply")
    def apply(x:Int) = x * 2
    @target("unapply")
    def unapply(z:Int) = if (z % 2 == 0) Some(z/2) else None
  }
 }

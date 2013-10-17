package tests.tca.matching

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object Extractor {

   /**
    * Testing extractors
    */
  
  @invocations("15: <unannotated> tests.tca.matching.Extractor.Twice: apply(x: Int)",
               "17: <unannotated> tests.tca.matching.Extractor.Twice: unapply(z: Int)")
   def main(args: Array[String]) {
     val x = Twice(21)
     x match {
       case Twice(y) => {"foo"; this}.foo()
       case _        => Console.println("wrong")
     }
   }

   @target("foo") def foo() {
     Console.println("foo")
   }

  object Twice {
    def apply(x:Int) = x * 2
    def unapply(z:Int) = if (z % 2 == 0) Some(z/2) else None
  }
 }

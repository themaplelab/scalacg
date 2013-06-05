package tests.matching

import callgraph.annotation.target

object Extractor {

   /**
    * Testing extractors
    */
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

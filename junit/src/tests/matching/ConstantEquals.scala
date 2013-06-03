package tests.matching

import tests.target
import tests.invocations

object ConstantEquals {

   /**
    * Testing extractors
    */
   @invocations("equals")
   def main(args: Array[String]) {
    Nil match {
      case Nil => println("right")
      case _ => println("wrong")
    }
  }
 }

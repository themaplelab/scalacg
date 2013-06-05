package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object ConstantEquals {

   /**
    * Testing extractors
    */
   @invocations("<unannotated> scala.collection.immutable.Nil: equals()")
   def main(args: Array[String]) {
    Nil match {
      case Nil => println("right")
      case _ => println("wrong")
    }
  }
 }

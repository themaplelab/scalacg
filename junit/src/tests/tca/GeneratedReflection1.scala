package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object GeneratedReflection1 {
  class A {  }
  
  @invocations("12: <unannotated> scala.Any: toString()") 
  def main(args : Array[String]){
	  val a1 = new A { var b : Int = 0 }
	  a1.b.toString // note: the Scala compiler generates reflective code for the call to toString(). 
  }
}
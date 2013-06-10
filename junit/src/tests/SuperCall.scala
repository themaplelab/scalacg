package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall {

   trait X {
       @target("X.bar") def bar(){ println("X.bar"); }
    }
  
	trait Y extends X {
	   @target("Y.foo")
	   @invocations("15: X.bar, Z.bar")
	   def foo() = { 
	     super.bar(); // { "X.bar"; "Z.bar"; super }.bar(); is not legal Scala code
	   }  
	}
	
	trait Z extends X {
	  @target("Z.bar") override def bar(){  println("Z.bar"); }
	} 
  
  def main(args: Array[String]): Unit = {
      { "Y.foo"; (new Y with Z)}.foo(); // calls X.bar
	  { "Y.foo"; (new Z with Y)}.foo(); // calls Z.bar
	  
	  { "FORCE_TEST_FAILURE"; this}.fail(); // to make sure that the test fails until the @invocations are checked
  }
  
  def fail(){}

}
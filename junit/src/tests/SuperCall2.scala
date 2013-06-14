package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall2 {
  
  trait X {
    def bar() : String;
  }
  
  trait Y extends X {    
    abstract override def bar() = super.bar();  
  } 
  
  trait Z extends X {
     def bar() = "Z.bar";
  }
  
  trait W extends X {
    def bar() = "W.bar";
  }
  
  def main(args: Array[String]) {
    val v1 = (new Z with Y).bar();
    println(v1); // prints "Z.bar"
    val v2 = (new W with Y).bar();
    println(v2); // prints "W.bar"
    
    { "FORCE_TEST_FAILURE"; this}.fail(); //  make sure that the test fails until the supercalls are checked
  }

  def fail(){}
  
}
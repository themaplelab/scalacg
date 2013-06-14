package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall3 {
  
  trait X {
    def bar() = "X.bar";
  }
  
  trait Y {
    def bar() = "Y.bar";
  }
  
  trait Z extends X with Y {    
    override def bar() = "Z.bar";
    def zip() = super[X].bar();  
    def zap() = super[Y].bar(); 
  } 
  
  trait W extends X {
    override def bar() = "W.bar";
  } 
  
  def main(args: Array[String]) {
    
    val z1 = new Z(){};
    val v1 = z1.zip();
    println(v1); // prints "X.bar"
    val v2 = z1.zap();
    println(v2); // prints "Y.bar" 
    
    { "FORCE_TEST_FAILURE"; this}.fail(); //  make sure that the test fails until the supercalls are checked
  }

  def fail(){}
  
}
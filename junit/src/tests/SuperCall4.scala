package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall4 {
  
  trait X {
    def bar() = "X.bar";
  }
  
  trait Y extends X {    
    abstract override def bar() = super[X].bar();  
    def baz() = super.bar();
    def zap() = super[X].bar();
  } 
  
  trait Z extends X {
     override def bar() = "Z.bar";
  }
  
  trait W extends X {
    override def bar() = "W.bar";
  }
  
  trait Q extends X {
    override def bar() = "Q.bar";
  }
  
  def main(args: Array[String]) {
    val v1 = (new Y with Z).bar();
    println(v1); // prints "Z.bar" 
    
    val v2 = (new Z with Y).bar();
    println(v2); // prints "X.bar"
    
    val v3 = (new Y with W).baz();
    println(v3); // prints "X.bar"
    
    val v4 = (new W with Y).baz();
    println(v4); // prints "W.bar"
    
    val v5 = (new Y with Q).zap();
    println(v5); // prints "X.bar"
    
    val v6 = (new Q with Y).zap();
    println(v6); // prints "X.bar"
    
    
   { "FORCE_TEST_FAILURE"; this}.fail(); //  make sure that the test fails until the supercalls are checked
  }

  def fail(){}
  
}
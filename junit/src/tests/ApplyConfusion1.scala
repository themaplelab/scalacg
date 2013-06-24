package tests

import scala.collection.immutable.LinearSeq

object ApplyConfusion1 {

  class A extends (Int => String) {
	  def apply(m: Int) = "hello";
  }
  
  class B {
    def foo(j : Int) = "goodbye";
    def bar(z : (Int) => String) = z(3); // calls scala/Function1.apply:(Ljava/lang/Object;)Ljava/lang/Object;
    def zap() = bar(foo); 
  }
  
  def main(args: Array[String]): Unit = {
    
    val x : (Int => String) = new A();
    val y = x(2); // calls scala/Function1.apply:(Ljava/lang/Object;)Ljava/lang/Object;
    println(y);
    
    val v = new B;
    val w = v.zap();
    println(w);
    
     { "FORCE_TEST_FAILURE"; this}.fail(); // force test failure until we decide what call graph we want to compute..
    
  }

  def fail(){}
}
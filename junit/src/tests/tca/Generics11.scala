package tests.tca

import callgraph.annotation.target;

object Generics11 {
  trait A[X] {
    @target("A.foo") def foo(x : X) = { }
  }
  
  class P[S] {
    
  }
  
   
  
  trait B[X <: P[AnyVal]] extends A[X] {
     @target("B.foo") override def foo(x : X) = { }
  }
  
  def main(args: Array[String]): Unit = {
    val b : A[P[AnyVal]] = new B[P[AnyVal]]{};
    
    var x : scala.AnyVal = 3;
    var y : P[AnyVal] = new P[AnyVal]();
    
    val a = new A[P[AnyVal]]{}; 
    
   { "A.foo"; "B.foo"; b}.foo( y );
  }
}
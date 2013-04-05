package p

import tests.target;

object Generics12 {
  trait A[X] {
    @target("A.foo") def foo(x : X) = { }
  }
  
  class P[+S] {
    
  }
  
   
  
  trait B[X <: P[AnyVal]] extends A[X] {
     @target("B.foo") override def foo(x : X) = { }
  }
  
  def main(args: Array[String]): Unit = {
    val b : A[P[AnyVal]] = new B[P[AnyVal]]{};
    
    var x : scala.AnyVal = 3;
    var y : P[AnyVal] = new P[Int]();
    
    val a = new A[P[AnyVal]]{};
    
   { "A.foo"; "B.foo"; b}.foo( y );
  }
}
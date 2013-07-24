package tests.tca

import callgraph.annotation.target
 

object Generics6 {
  trait A[X,Y] {
    @target("A.foo") def foo(x : X) = { }
  }
  
  trait B[X] extends A[X,Any] {
     @target("B.foo") override def foo(x : X) = { }
  }
  
  def main(args: Array[String]): Unit = {
    val b : A[Int,Any] = new B[Int]{};
    val a = new A[Int,Any]{};
    
   { "A.foo"; "B.foo"; b}.foo(3);
  }
}
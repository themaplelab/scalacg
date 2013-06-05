package p

import callgraph.annotation.target;

object Generics10 {
  trait A[X] {
    @target("A.foo") def foo(x : X) = { }
  }
  
  trait B[X <: AnyVal] extends A[X] {
     @target("B.foo") override def foo(x : X) = { }
  }
  
  def main(args: Array[String]): Unit = {
    val b : A[Int] = new B[Int]{};
    val a = new A[Int]{};
    
   { "A.foo"; "B.foo"; b}.foo(3);
  }
}
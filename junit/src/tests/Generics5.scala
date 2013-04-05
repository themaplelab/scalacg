package p

import tests.target

object Generics5 {
  trait A[X] {
    @target("A.foo") def foo(x : X) = { }
  }
  
  trait B[X] extends A[X] {
     @target("B.foo") override def foo(x : X) = { }
  }
  
  def main(args: Array[String]): Unit = {
    val b : A[Int] = new B[Int]{};
    
   { "A.foo"; "B.foo"; b}.foo(3);
  }
}
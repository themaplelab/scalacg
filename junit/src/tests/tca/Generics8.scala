package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object Generics8 {
  trait A[X] {
    @target("A.foo") def foo(x : X) = { }
  }
  
  trait B[X,Y] extends A[X] {
     @target("B.foo") override def foo(x : X) = { }
  }
  
  def main(args: Array[String]): Unit = {
    val b : A[Int] = new B[Int,Int]{};
    val a = new A[Int]{};
    
   { "A.foo"; "B.foo"; b}.foo(3);
  }
}
package tests.ra

import callgraph.annotation.target
import callgraph.annotation.invocations

object Implicits1 {
  class B(n : Int){
    def get : Int = n

    override def toString = "B[" + n + "]"
  } 
  class A(p : Int) {
    override def toString = "A[" + p + "]"
  }
  @target("B2A") implicit def B2A(b : B):A = new A(b.get*b.get)

  @target("printA") def printA(a : A){ 
    println(a)
  }
  
  @invocations("24: B2A")
  def main(args: Array[String]) {
     val b = new B(7);
     { "printA"; this}.printA(b); // prints "A[49]"
  }
}
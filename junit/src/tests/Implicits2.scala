package tests

import callgraph.annotation.target

object Implicits2 {
  class B(n : Int){
    def get : Int = n;
    override def toString() = "B[" + n + "]";
  } 
  // implicit defined in "companion object"
  object B {
	@target("B2A") implicit def BtoA(b : B):A = new A(b.get*b.get);    
  }
 
  
  
  class A(p : Int) {
    override def toString() = "A[" + p + "]";
  }
   
  def printA(a : A){ 
    println(a)
  }
  
  def main(args: Array[String]) {
     val b = new B(7);
     printA(b); // prints "A[49]"
  }
}
package tests.tca

import callgraph.annotation.target

object PathTypes7 {
  class A {
    trait B {
      def m();
    }
  }
  
  def main(args : Array[String]){
	  val a1 = new A { var b : B = null }
	  val a2 = new A { var b : B = null }
	  
	  a1.b = new a1.B {  @target("a1.B.m") override def m() = println("a1.B.m") }
	  a2.b = new a2.B {  @target("a2.B.m") override def m() = println("a2.B.m") }
//	  a1.b = a2.b; // Note: type-incorrect in Scala
	
	  { "a1.B.m"; a1.b }.m();
	  { "a2.B.m"; a2.b }.m();
  }
}
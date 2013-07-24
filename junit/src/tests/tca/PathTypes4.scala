package tests.tca

import callgraph.annotation.target

object PathTypes4 {
  class A {
    trait B {
      def m();
    }
  }
  val a1 = new A { var a1b: B = null }
  val a2 = new A { var a2b: B = null }
  val b1 = new a1.B { @target("a1.B.m") override def m() = println("a1.B.m") }
  val b2 = new a2.B { @target("a2.B.m") override def m() = println("a2.B.m") }
  
  def main(args : Array[String]){
	  a1.a1b = b1
	  a2.a2b = b2
//	  a1.a1b = b2; // Note: type-incorrect in Scala
	
	  { "a1.B.m"; a1.a1b }.m();
	  { "a2.B.m"; a2.a2b }.m();
  }
}
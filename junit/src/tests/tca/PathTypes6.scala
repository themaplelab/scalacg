package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object PathTypes6 {
  class A {
    trait B {
      def m();
    }
    var b : B = null;
  }
  
  def main(args : Array[String]){
	  val a1 = new A
	  val a2 = new A 
	  a1.b = new a1.B { @target("a1.B.m") override def m() = println("a1.B.m") }
	  a2.b = new a2.B { @target("a2.B.m") override def m() = println("a2.B.m") }
//	  a1.b = a2.b; // Note: type-incorrect in Scala
	
	  { "a1.B.m"; a1.b }.m();
	  { "a2.B.m"; a2.b }.m();
  }
}
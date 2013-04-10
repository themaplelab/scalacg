package tests

object PathTypes5 {
  class A {
    trait B {
      def m();
    }
    var b : B = null;
  }
  val a1 = new A
  val a2 = new A
  val b1 = new a1.B { @target("a1.B.m") override def m() = println("a1.B.m") }
  val b2 = new a2.B { @target("a2.B.m") override def m() = println("a2.B.m") }
  
  def main(args : Array[String]){
	  a1.b = b1
	  a2.b = b2
//	  a1.b = b2; // Note: type-incorrect in Scala
	
	  { "a1.B.m"; a1.b }.m();
	  { "a2.B.m"; a2.b }.m();
  }
}
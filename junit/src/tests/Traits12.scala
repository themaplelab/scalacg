package tests

object Traits12 {
  
  trait A {
    @target("A.foo") def foo() : Unit = { println("B.bar"); }
    def bar() : Unit;
  }
  
  trait B {
    @target("B.bar") def bar() : Unit = { { "A.foo"; this }.foo(); }
    def foo() : Unit;
  }
  
  def main(args : Array[String]) : Unit = {
     { "B.bar"; (new A with B) }.bar();
  }
  
}
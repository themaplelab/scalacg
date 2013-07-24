package tests.tca

import callgraph.annotation.target

object MultipleBounds {
  
  trait HasFoo {
    def foo : Unit;
  }
  
  trait HasBar {
    def bar : Unit;
  }
   
  trait X {
    type A <: HasFoo
    val x : A;
  }
  
  trait Y {
    type A <: HasBar
    val y : A;
  }
  
  trait Z {
    class C extends HasFoo with HasBar {
      @target("C.foo") def foo() = { println("C.foo") }
      @target("C.bar") def bar() = { println("C.bar") }
    }
    type A = C
    val x = new C;
    val y = new C;
  }
  
  
  def main(args: Array[String]) {
     val p = new X with Y with Z;
     { "C.foo"; p.x}.foo();
     { "C.bar"; p.x}.bar();
     { "C.foo"; p.y}.foo();
     { "C.bar"; p.y}.bar();
  }
}
package tests

import callgraph.annotation.target


object SimpleTraits {
  def main(args: Array[String]) = {
    new B();
    new C();
  }

  trait A {
    @target("A.foo") def foo();
    
    @target("A.bar") def bar(): Unit = {
      { "B.foo"; this }.foo();
      println("A.bar()");
    }
  }
  
  class B extends A {
    @target("B.foo") def foo() : Unit = { println("B.foo()") }
  }
  
  class C extends A {
    @target("C.foo") def foo() : Unit = { println("C.foo()") }
    @target("C.bar") override def bar() : Unit = {
      
    }
  }
}
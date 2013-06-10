package tests

import callgraph.annotation.target
import callgraph.annotation.notreachable


object Simple {
  def main(args: Array[String]) : Unit = {
    new B();
    new C();
  }

  trait A {
    @notreachable @target("A.foo") def foo();
    
    @notreachable @target("A.bar") def bar(): Unit = {
      { "__NONE__"; this }.foo();
      println("A.bar()");
    }
  }
  
  class B extends A {
    @notreachable @target("B.foo") def foo() : Unit = { println("B.foo()") }
  }
  
  class C extends A {
    @notreachable @target("C.foo") def foo() : Unit = { println("C.foo()") }
    @notreachable @target("C.bar") override def bar() : Unit = {
      
    }
  }
}
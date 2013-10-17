package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object Traits10 {
  trait A { self => // self refers to this
    def foo();
    @target("A.bar") def bar() { { "B.foo"; self }.foo(); } // can only call B.foo() because that is the only type with a def. of foo() that can have A.bar() as a member
  }

  class B extends A {
    @target("B.foo") def foo() {}
  }

  class C extends A {
    @target("C.foo") def foo() {}
    @target("C.bar") override def bar() {}
  }
  
  def main(args: Array[String]) = {
    { "A.bar"; (new B)}.bar;
    { "C.bar"; (new C)}.bar
  }
} 
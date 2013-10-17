package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.notreachable
import ca.uwaterloo.scalacg.annotation.reachable
import ca.uwaterloo.scalacg.annotation.invocations

object ThisType2b {
  trait A {
    def foo();
    @target("A.bar") def bar() {
      var x: this.type = this;
      { "B.foo"; "C.foo"; x }.foo(); // can call B.foo() or C.foo() --- the run-time type of this might be "C" because there is a super-call to A.bar()
    }
  }

  class B extends A {
    @target("B.foo") def foo() {}
  }

  class C extends A {
    @target("C.foo") def foo() {}
    @target("C.bar") override def bar() {}
    
    @reachable
    @invocations("28: A.bar")
    @target("C.baz") def baz() {
      super[A].bar();
    }
  }

  def main(args: Array[String]) = {
    { "A.bar"; (new B)}.bar;
    { "C.bar"; (new C)}.bar;
    { "C.baz"; (new C)}.baz
    
  }
} 
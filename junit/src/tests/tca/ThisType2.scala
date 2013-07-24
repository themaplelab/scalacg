package tests.tca

import callgraph.annotation.target
import callgraph.annotation.notreachable
import callgraph.annotation.reachable

object ThisType2 {
  trait A {
    def foo();
    @target("A.bar") def bar() {
      var x: this.type = this;
      { "B.foo"; x }.foo(); // can call B.foo() only --- the run-time type of this cannot be be "C" because the super-call to A.bar() is in unreachable code
    }
  }

  class B extends A {
    @target("B.foo") def foo() {}
  }

  class C extends A {
    @target("C.foo") def foo() {}
    @target("C.bar") override def bar() {}
    
    @notreachable
    def baz() {
      super[A].bar();
    }
  }

  def main(args: Array[String]) = {
    { "A.bar"; (new B)}.bar;
    { "C.bar"; (new C)}.bar
  }
} 
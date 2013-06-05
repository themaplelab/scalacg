package tests

import callgraph.annotation.target

object ThisType2 {
  trait A {
    def foo();
    def bar() {
      var x: this.type = this;
      { "B.foo"; "C.foo"; x }.foo(); // can call B.foo() or C.foo() --- the run-time type of this might be "C" because there is a super-call to A.bar()
    }
  }

  class B extends A {
    @target("B.foo") def foo() {}
  }

  class C extends A {
    @target("C.foo") def foo() {}
    override def bar() {}
    def baz() {
      super[A].bar();
    }
  }

  def main(args: Array[String]) = {
    (new B).bar
    (new C).bar
  }
} 
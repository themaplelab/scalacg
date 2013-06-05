package tests

import callgraph.annotation.target

object Traits9 {
  trait A {
    def foo();
    def bar() { { "B.foo"; "C.foo"; this }.foo(); } // can only call B.foo() or C.foo() --- the run-time type of this might be "C" because there is a super-call to A.bar()
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
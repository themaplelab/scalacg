package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations
import ca.uwaterloo.scalacg.annotation.notreachable

object Traits9 {
  trait A {
    def foo();
    @target("A.bar") def bar() { { "B.foo"; "C.foo"; this }.foo(); } // can only call B.foo() or C.foo() --- the run-time type of this might be "C" because there is a super-call to A.bar()
  }

  class B extends A {
    @target("B.foo") def foo() {}
  }

  class C extends A {
    @target("C.foo") def foo() {}
    @target("C.bar") override def bar() {}
    
    @invocations("23: A.bar")
    @target("C.baz") def baz() {
      super[A].bar();
    }
  }
  def main(args: Array[String]) = {
    { "A.bar"; (new B)}.bar;
    { "C.baz"; (new C)}.baz
  }

} 
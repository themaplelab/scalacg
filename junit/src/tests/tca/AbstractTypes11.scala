package tests.tca

import callgraph.annotation.target
import callgraph.annotation.notreachable

object AbstractTypes11 {
  trait A {
    type T <: X
    val field: T
    @target("A.foo") def foo() = { "C.bar"; field }.bar()
  }

  class B extends A {
    type T = C
    val field: T = new C()
  }

  trait X {
    def bar(): Unit
  }

  class C extends X {
    @target("C.bar") override def bar() = {}
  }

  class D extends X {
    @notreachable @target("D.bar") override def bar() = {}
  }
  def main(args: Array[String]): Unit = {
    { "A.foo"; new B}.foo;
    new C
    new D
  }
}
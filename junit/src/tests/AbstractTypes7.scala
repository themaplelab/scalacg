package tests

import callgraph.annotation.target

object AbstractTypes7 {
  abstract class A {
    type T <: C
    var foo: T

    def bar() =
      // call a method on an abstract type
      { "C.bar"; "D.bar"; foo }.bar()
  }

  class B extends A {
    // instantiate the type to the upper bound and
    // instantiate the variable as a subclass
    type T = C
    var foo: C = new D
  }

  class C {
    @target("C.bar") def bar() = "C"
  }

  class D extends C {
    @target("D.bar") override def bar() = "D"
  }

  def main(args: Array[String]): Unit = {
    new B().bar()
    new C()
    new D()
  }
}
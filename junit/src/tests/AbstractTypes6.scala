package tests

import callgraph.annotation.target

object AbstractTypes6 {
  abstract class A {
    type T <: C
    var foo: T

    @target("A.bar") def bar() =
      // call a method on an abstract type
      { "C.bar"; "D.bar"; foo }.bar()
  }

  class B extends A {
    // instantiate the type to the upper bound
    type T = C
    var foo = new C
  }

  class C {
    @target("C.bar") def bar() = "C"
  }

  class D extends C {
    @target("D.bar") override def bar() = "D"
  }

  def main(args: Array[String]): Unit = {
    { "A.bar"; new B()}.bar()
    new C()
    new D()
  }
}
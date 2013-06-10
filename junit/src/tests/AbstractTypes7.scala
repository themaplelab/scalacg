package tests

import callgraph.annotation.target
import callgraph.annotation.notreachable

object AbstractTypes7 {
  abstract class A {
    type T <: C
    var foo: T

    @target("A.bar") def bar() =
      // call a method on an abstract type
      { "D.bar"; foo }.bar()
  }

  class B extends A {
    // instantiate the type to the upper bound and
    // instantiate the variable as a subclass
    type T = C
    var foo: C = new D
  }

  class C {
    @notreachable @target("C.bar") def bar() = "C"
  }

  class D extends C {
    @target("D.bar") override def bar() = "D"
  }

  def main(args: Array[String]): Unit = {
    { "A.bar"; new B()}.bar()
  }
}
package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.notreachable

object AbstractTypes8 {
  abstract class A {
    type T <: C
    var foo: T

    @target("A.bar") def bar() =
      // call a method on an abstract type
      { "D.bar"; foo }.bar()
  }

  class B extends A {
    // instantiate the type var as a subclass
    type T = D
    var foo = new D
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
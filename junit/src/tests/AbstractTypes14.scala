package p

import tests.target

object AbstractTypes14 {
  trait HasFoo {
    def foo: String
  }
  trait X {
    class A extends HasFoo {
       @target("X.A.foo") def foo = "X.A.foo"
    }
  }
  trait Y {
    class A extends HasFoo {
      @target("Y.A.foo") def foo = "Y.A.foo"
    }
    type B = A
    val o = new A
  }
  trait Z {
    type B <: HasFoo
    val o: B
    def go = println({ "Y.A.foo"; o}.foo)
  }

  def main(args: Array[String]) {
    (new Y with Z {}).go
  }
}
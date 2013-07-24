package tests.tca

import callgraph.annotation.target
import callgraph.annotation.notreachable

object AbstractTypes13 {
  trait HasFoo {
    def foo: String
  }
  
  class R {
    class A extends HasFoo {
      @notreachable @target("R.A.foo") def foo = "R.A.foo"
    }
  }

  trait X {
    class A extends HasFoo {
      @target("X.A.foo") def foo = "X.A.foo"
    }
    val o = new A
  }

  trait Y {
    type A
    type B = A
  }

  trait Z {
    type B <: HasFoo
    val o: B
    @target("Z.go") def go = println({ "X.A.foo"; o}.foo)
  }

  def main(args: Array[String]) {
    { "Z.go"; (new X with Y with Z {})}.go
  }

}
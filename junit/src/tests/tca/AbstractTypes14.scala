package tests.tca

import callgraph.annotation.target
import callgraph.annotation.notreachable
 
object AbstractTypes14 {
  trait HasFoo {
    def foo: Unit
  }
  trait X {
    class A extends HasFoo {
       @notreachable @target("X.A.foo") def foo = println("X.A.foo")
    }
  }
  trait Y {
    class A extends HasFoo {
      @target("Y.A.foo") def foo = println("Y.A.foo")
    }
    type B = A
    val o = new A
  }
  trait Z {
    type B <: HasFoo
    val o: B
    @target("Z.bar") def bar = { "Y.A.foo"; o}.foo
  }

  def main(args: Array[String]) {
    { "Z.bar"; (new Y with Z {})}.bar
  }
}
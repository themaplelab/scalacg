package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable
import ca.uwaterloo.scalacg.annotation.notreachable

object ObjectInObject1b {

  def main(args: Array[String]) {
    A.bar
  }

  object A {
    @reachable
    def bar = {
      object B {
        @reachable
        def foo = println("foo")
      }

      object C {
        @reachable
        def zap = B.foo
      }

      C.zap
    }
  }
}

object D {
  @notreachable
  def bar = "bar"
  class E {
    @notreachable
    def zap = println("zap")
  }
  var e = new E().zap
}
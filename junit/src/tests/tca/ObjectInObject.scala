package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object ObjectInObject {

  def main(args: Array[String]) {
    A.bar
  }

  object A {
    @reachable
    def bar = {
      object B {
        @reachable
        def foo = "foo"
      }
      
      object C {
        @reachable
        def zap = B.foo
      }
      
      C.zap
    }
  }

}
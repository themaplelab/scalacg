package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall1 {
//todo replace bar with bar
  trait X {
    @target("X.bar")
    def bar() { println("X.bar"); }
  }

  trait Y extends X {
    @target("Y.foo")
    @invocations("17: X.bar", "17: Z.bar")
    def foo() {
      super.bar(); // { "X.bar"; "Z.bar"; super }.bar(); is not legal Scala code
    }
  }

  trait Z extends X {
    @target("Z.bar") override def bar() { println("Z.bar"); }
  }
  
  def main(args: Array[String]) {
    {"Y.foo"; (new Y with Z)}.foo(); // calls X.bar
    {"Y.foo"; (new Z with Y)}.foo(); // calls Z.bar
  }
}
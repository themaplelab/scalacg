package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall1 {

  trait X {
    @target("X.baz")
    def baz() { println("X.baz"); }
  }

  trait Y extends X {
    @target("Y.foo")
    @invocations("17: X.baz", "17: Z.baz")
    def foo() {
      super.baz(); // { "X.baz"; "Z.baz"; super }.baz(); is not legal Scala code
    }
  }

  trait Z extends X {
    @target("Z.baz") override def baz() { println("Z.baz"); }
  }

  def main(args: Array[String]) {
    {"Y.foo"; (new Y with Z)}.foo(); // calls X.baz
    {"Y.foo"; (new Z with Y)}.foo(); // calls Z.baz
  }
}
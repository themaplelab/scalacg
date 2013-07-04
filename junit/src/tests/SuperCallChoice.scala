package tests

import callgraph.annotation.{ reachable, invocations, target }

object SuperCallChoice {

  trait X {
    @target("X.baz")
    def baz() { println("X.baz"); }
  }

  trait Y extends X with Z {
    @target("Y.foo")
    @invocations("16: X.baz")
    def foo() {
      super[X].baz(); // without [X], super would refer to Z
    }
  }

  trait Z extends X {
    @target("Z.baz") override def baz() { println("Z.baz"); }
  }

  def main(args: Array[String]) {
    { "Y.foo"; (new Y with X) }.foo(); // "X.baz"
  }
}

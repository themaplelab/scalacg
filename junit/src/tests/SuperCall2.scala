package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall2 {
  trait X {
    @target("X.bar")
    def bar(): String;
  }

  trait Y extends X {
    @invocations("14: Z.bar", "14: W.bar")
    abstract override def bar() = super.bar()
  }

  trait Z extends X {
    @target("Z.bar")
    def bar() = "Z.bar"
  }

  trait W extends X {
    @target("W.bar")
    def bar() = "W.bar"
  }

  def main(args: Array[String]) {
    val v1 = (new Z with Y).bar()
    println(v1) // prints "Z.bar"
    val v2 = (new W with Y).bar()
    println(v2) // prints "W.bar"
  }
}
package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall2 {
  //todo: replace baz to bar!
  trait X {
    @target("X.baz")
    def baz(): String;
  }

  trait Y extends X {
    @invocations("15: Z.baz", "15: W.baz")
    abstract override def baz() = super.baz()
  }

  trait Z extends X {
    @target("Z.baz")
    def baz() = "Z.baz"
  }

  trait W extends X {
    @target("W.baz")
    def baz() = "W.baz"
  }

  def main(args: Array[String]) {
    val v1 = (new Z with Y).baz()
    println(v1) // prints "Z.baz"
    val v2 = (new W with Y).baz()
    println(v2) // prints "W.baz"
  }
}
package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall2 {
  trait X {
    @target("X.plus")
    def plus(): String;
  }

  trait Y extends X {
    @invocations("15: Z.plus", "15: W.plus")
    abstract override def plus() = super.plus()
  }

  trait Z extends X {
    @target("Z.plus")
    def plus() = "Z.plus"
  }

  trait W extends X {
    @target("W.plus")
    def plus() = "W.plus"
  }

  def main(args: Array[String]) {
    val v1 = (new Z with Y).plus()
    println(v1) // prints "Z.plus"
    val v2 = (new W with Y).plus()
    println(v2) // prints "W.plus"
  }
}
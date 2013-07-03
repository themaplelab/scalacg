package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall3 {
  //todo: replace baz back to bar
  trait X {
    @target("X.baz")
    def baz() = "X.baz"
  }

  trait Y {
    @target("Y.baz")
    def baz() = "Y.baz"
  }

  trait Z extends X with Y {
    override def baz() = "Z.baz"

    @invocations("22: X.baz")
    def zip() = super[X].baz()

    @invocations("25: Y.baz")
    def zap() = super[Y].baz()
  }

  trait W extends X {
    @target("W.baz")
    override def baz() = "W.baz";
  }

  def main(args: Array[String]) {
    val z1 = new Z() {};
    val v1 = z1.zip();
    println(v1); // prints "X.baz"
    val v2 = z1.zap();
    println(v2); // prints "Y.baz" 
  }
}
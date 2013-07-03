package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall3 {
  trait X {
    @target("X.bar")
    def bar() = "X.bar"
  }

  trait Y {
    @target("Y.bar")
    def bar() = "Y.bar"
  }

  trait Z extends X with Y {
    override def bar() = "Z.bar"

    @invocations("22: X.bar")
    def zip() = super[X].bar()

    @invocations("25: Y.bar")
    def zap() = super[Y].bar()
  }

  trait W extends X {
    @target("W.bar")
    override def bar() = "W.bar";
  }

  def main(args: Array[String]) {
    val z1 = new Z() {};
    val v1 = z1.zip();
    println(v1); // prints "X.bar"
    val v2 = z1.zap();
    println(v2); // prints "Y.bar" 
  }
}
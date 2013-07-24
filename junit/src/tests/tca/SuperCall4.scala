package tests.tca

import callgraph.annotation.target
import callgraph.annotation.invocations

object SuperCall4 {
  //todo: replace bam back to bar
  trait X {
    @target("X.bam")
    def bam() = "X.bam"
  }

  trait Y extends X {
    @invocations("15: X.bam")
    abstract override def bam() = super[X].bam()

    @invocations("18: X.bam")
    def baz() = super.bam()

    @invocations("21: X.bam")
    def zap() = super[X].bam()
  }

  trait Z extends X {
    override def bam() = "Z.bam"
  }

  trait W extends X {
    override def bam() = "W.bam"
  }

  trait Q extends X {
    override def bam() = "Q.bam"
  }

  def main(args: Array[String]) {
    val v1 = (new Y with Z).bam()
    println(v1) // prints "Z.bam"

    val v2 = (new Z with Y).bam()
    println(v2) // prints "X.bam"

    val v3 = (new Y with W).baz()
    println(v3) // prints "X.bam"

    val v4 = (new W with Y).baz()
    println(v4) // prints "W.bam"

    val v5 = (new Y with Q).zap()
    println(v5) // prints "X.bam"

    val v6 = (new Q with Y).zap()
    println(v6) // prints "X.bam"
  }
}

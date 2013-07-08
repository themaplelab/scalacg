package tests

import callgraph.annotation.invocations
import callgraph.annotation.target
import callgraph.annotation.reachable

object MultipleAnnotations {

  /*
   * Karim: see Reachable1
   * @invocations("13: A.apply")
   * @invocations("18: B.apply")
   */
  def main(args: Array[String]) = {
    // This calls A.apply
    for (v <- 0 to 10) {
      A(v)
    }

    // This calls B.apply
    for (v <- 0 to 10) {
      B(v)
    }
  }

  object A {
    @target("A.apply")
    @reachable
    def apply(x: Int) = { "A" + x }
  }

  object B {
    @target("B.apply")
    @reachable
    def apply(x: Int) = { "B" + x }
  }

}
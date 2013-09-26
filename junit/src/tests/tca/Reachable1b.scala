package tests.tca

import callgraph.annotation.notreachable
import callgraph.annotation.reachable

object Reachable1b {

  /*
   * Karim: testing the generation of duplicate anonymous function classes
   */
  def main(args: Array[String]): Unit = {

    // This calls A.apply
    for (v <- 0 to 10) {
      A(v)
    }

    // This calls A.apply
    for (v <- 0 to 10) {
      A(v)
    } 

  }

  object A {
    @reachable
    def apply(x: Int) = { "A" + x }
  }

  object B {
    @notreachable
    def apply(x: Int) = { "B" + x }
  }

}
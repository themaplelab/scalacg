package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable
import ca.uwaterloo.scalacg.annotation.invocations

object Reachable1 {

  /*
   * Karim: Commenting out because they now are in the uncurried apply methods. 
   * @invocations("13: <unannotated> tests.Reachable1.A: apply(x: Int)", "18: <unannotated> tests.Reachable1.B: apply(x: Int)")
   */
  def main(args: Array[String]): Unit = {

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
    @reachable
    def apply(x: Int) = { "A" + x }
  }

  object B {
    @reachable
    def apply(x: Int) = { "B" + x }
  }

}
package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object LiftedMethod1 {

  def main(args: Array[String]) {
    { "foo"; this}.foo()
  }

  @invocations("17: <unannotated> tests.tca.LiftedMethod1: toText(i: Int)")
  @target("foo") 
  def foo() {
    def toText(i: Int) = empty + i
    def empty = ""
    val v = toText(1)
  }

}
package tests

import callgraph.annotation.reachable
import callgraph.annotation.invocations

object ToString1 {

  @invocations("10: <unannotated> tests.ToString1.A: <init>()")
  def main(args: Array[String]) = {
    val a = new A
    println(a)
  }

  class A extends B {
    @reachable def name = "class A"
    def bla = "bla"
    @reachable override def toString = {
      val v = name
      v
    }
  }

  abstract class B {
    def bla: String
  }

}
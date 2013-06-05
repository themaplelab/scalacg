package tests

import callgraph.annotation.reachable

object ToString1 {

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
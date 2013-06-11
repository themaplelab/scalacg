package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

/*
* Test setter overloading
* */

object SetterOverloading {

  class X(private var _i: Int) {
    def i = _i

    @target("asgn")
    def i_=(i: Int) {
      println(i)
      _i = i
    }
  }

  @invocations("24: asgn")
  def main(args: Array[String]) {
    new X(1).i = 3
  }
}
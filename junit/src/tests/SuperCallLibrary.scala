package tests

import callgraph.annotation.target
import callgraph.annotation.invocations
import scala.collection.GenSet

object SuperCallLibrary {

  class A {}

  trait X {
    @invocations("13: <unannotated> java.lang.Object: toString()")
    def bar = super.toString
  }

  def main(args: Array[String]) {
    (new A with X).bar
  }
}
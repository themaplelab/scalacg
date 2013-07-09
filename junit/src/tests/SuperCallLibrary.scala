package tests

import scala.collection.immutable.LinearSeq

import callgraph.annotation.invocations

object SuperCallLibrary {

  trait A extends X {
    @invocations("11: <unannotated> java.lang.Object: toString()")
    def foo = super.toString
  }

  trait X {
    @invocations("16: <unannotated> java.lang.Object: toString()")
    def bar = super.toString
  }

  def main(args: Array[String]) {
    (new A with X).bar
    (new A with X).foo
  }
}
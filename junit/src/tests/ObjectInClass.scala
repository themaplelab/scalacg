package tests

import callgraph.annotation.reachable

object ObjectInClass {

  def main(args: Array[String]) {
    val a = new A
    val b = a.B
    b.foo
  }
  
  class A {
    object B {
      @reachable
      def foo = "foo"
    }
  }

}
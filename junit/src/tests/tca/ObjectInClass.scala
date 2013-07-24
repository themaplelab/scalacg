package tests.tca

import callgraph.annotation.reachable

object ObjectInClass {

  def main(args: Array[String]) {
    val a = new A
    val b = a.B
    b.foo
    val c = a.c
    c.bar
  }
  
  class A {
    object B {
      @reachable
      def foo = "foo"
    }
    class C {
      @reachable
      def bar = "foo"
    }
    val c = new C
  }

}
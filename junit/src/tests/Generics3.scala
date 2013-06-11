package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object Generics3 {
  trait A[T <: X] {
    var field: T
    @target("A.foo") def foo() = { "C.bar"; field }.bar()
  }

  @invocations("13: field_=")
  @target("Generics3.foo") def foo(ac: A[C]) = {
    ac.field = new C()
  }

  trait X {
    def bar(): Unit
  }

  class C extends X {
    @target("C.bar") override def bar() = {}
  }

  class D extends X {
    @target("D.bar") override def bar() = {}
  }
  def main(args: Array[String]) = {
    val a = new A[C] {
      var field = null
      
      def field_=(c: C) = field = c
    }
    {"Generics3.foo"; this}.foo(a)
  }
}
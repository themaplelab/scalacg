package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object Generics3 {
  trait A[T <: X] {
    var field: T
    @target("A.foo") def foo() { { "C.bar"; field }.bar() }
  }

  // See my comment in the main method for Generics3.scala
  //  @invocations("16: field_=")
  @invocations("16: <unannotated> tests.tca.Generics3.C: <init>()")
  @target("Generics3.foo") def foo(ac: A[C]) {
    ac.field = new C()
  }

  trait X {
    def bar()
  }

  class C extends X {
    @target("C.bar") override def bar() {}
  }

  class D extends X {
    @target("D.bar") override def bar() {}
  }
  def main(args: Array[String]) {
    val a = new A[C] {
      var field = null
      
      def field_=(c: C) { field = c }
    }
    {"Generics3.foo"; this}.foo(a)
  }
}
package tests.ra

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.notreachable

object Generics1 {

  class C {
    @target("C.foo")
    def foo() = "C"
  }

  class D extends C {
    @target("D.foo")
    override def foo() = "D"
  }

  class A[T <: C](var elem: T) {
    @target("A.bar")
    def bar() = {"C.foo"; "D.foo"; elem}.foo()
  }

  def main(args: Array[String]) {
    {"A.bar"; new A[C](new C)}.bar()
  }
}
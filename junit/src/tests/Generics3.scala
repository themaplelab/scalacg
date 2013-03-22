package tests

object Generics3 {
  trait A[T <: X] {
    var field: T
    def foo() = { "C.bar"; field }.bar()
  }

  def foo(ac: A[C]) = {
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
    foo(a)
  }
}
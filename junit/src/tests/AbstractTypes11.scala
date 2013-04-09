package tests

object AbstractTypes11 {
  trait A {
    type T <: X
    val field: T
    def foo() = { "C.bar"; field }.bar()
  }

  class B extends A {
    type T = C
    val field: T = new C()
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
  def main(args: Array[String]): Unit = {
    new B
    new C
    new D
  }
}
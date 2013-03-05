package tests

object AbstractTypes5 {
  abstract class A {
    type T <: C
    var foo: T

    def bar() =
      { "C.toString"; "D.toString"; foo }.toString()
  }

  class B extends A {
    override type T = D
    var foo = new D
  }

  class C {
    @target("C.toString") override def toString() = "C"
  }

  class D extends C {
    @target("D.toString") override def toString() = "D"
  }

  new B().bar()
}
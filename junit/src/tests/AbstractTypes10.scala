package tests

object AbstractTypes10 {
  trait A {
    type T
    val field: T
    def foo() = { "C.hashCode"; field }.hashCode()
  }

  class B extends A {
    type T = C
    val field: T = new C()
  }

  class C {
    @target("C.hashCode") override def hashCode() = 42
  }

  class D {
    @target("D.hashCode") override def hashCode() = 23
  }
  def main(args: Array[String]): Unit = {
    new B()
    new C()
    new D()
  }
}
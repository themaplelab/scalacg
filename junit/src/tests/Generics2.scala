package tests

object Generics2 {
  trait A[T] {
    var field: T
    def foo() = { "C.hashCode"; field }.hashCode()
  }

  def foo(ac: A[C]) = {
    ac.field = new C();
  }

  class C {
    @target("C.hashCode") override def hashCode() = 42
  }

  class D {
    @target("D.hashCode") override def hashCode() = 23
  }

  def main(args: Array[String]) = {
    val a = new A[C] {
      var field = null
      def field_=(c: C) = field = c
    }
    foo(a)
  }
}
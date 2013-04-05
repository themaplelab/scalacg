package tests

object Generics15 {
  trait A[T] {
    def foo(): String
    def bar(t: T): String = { { "B.foo"; this }.foo() }
  }
  class B[T <: Int] extends A[T] {
    @target("B.foo") def foo(): String = "foo"
  }
  class C extends B[Int]
  def main(args: Array[String]): Unit = {
    (new B[Int]()).bar(1)
    (new C()).bar(1)
  }

}
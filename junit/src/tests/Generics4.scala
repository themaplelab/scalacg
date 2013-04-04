package tests

object Generics4 {
  trait A[T] {
    def foo: String
    def bar: String = { { "B.foo"; this }.foo }
  }
  class B[T] extends A[T] {
    @target("B.foo") def foo: String = "foo"
  }
  def main(args: Array[String]): Unit = {
    (new B[Int]()).bar
  }

}
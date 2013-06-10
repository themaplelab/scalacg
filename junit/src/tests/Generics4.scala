package tests

import callgraph.annotation.target

object Generics4 {
  trait A[T] {
    def foo: String
    @target("A.bar") def bar: String = { { "B.foo"; this }.foo }
  }
  class B[T] extends A[T] {
    @target("B.foo") def foo: String = "foo"
  }
  def main(args: Array[String]): Unit = {
    { "A.bar"; (new B[Int]())}.bar
  }

}
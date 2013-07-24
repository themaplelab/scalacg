package tests.tca

import callgraph.annotation.target

object Generics15 {
  trait A[T] {
    def foo(): String
     @target("A.bar") def bar(t: T): String = { { "B.foo"; this }.foo() }
  }
  class B[T <: Int] extends A[T] {
    @target("B.foo") def foo(): String = "foo"
  }
  class C extends B[Int]
  def main(args: Array[String]): Unit = {
    { "A.bar"; (new B[Int]())}.bar(1);
    { "A.bar"; (new C())}.bar(1)
  }

}
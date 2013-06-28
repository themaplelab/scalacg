package tests

import callgraph.annotation.target

object Nested1 {
  trait A1 {
    trait C1 {
      def foo(): String
    }
    val c: C1
    def go = { "C2.foo"; c }.foo
  }
  trait A2 extends A1 {
    trait C2 extends C1 {
      @target("C2.foo") def foo() = "foo"
    }
    val c = new C2 {}
  }
  def main(args: Array[String]): Unit = {
    val a2 = new A2 {}
    a2.go
  }
}
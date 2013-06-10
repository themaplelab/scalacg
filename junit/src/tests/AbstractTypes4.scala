package tests

import callgraph.annotation.target
import callgraph.annotation.notreachable

object AbstractTypes4 {
  def main(args: Array[String]): Unit = {
    val e: E = new E {
      type T = List[U]
      val element = List(-1, 3, -5, 7, -9)
    }
    { "E.foo"; e }.foo();
  }

  abstract class A {
    type T
    val element: T
  }

  abstract class B extends A {
    type U
    type T <: List[U]
    @notreachable @target("B.foo") def foo(): U
  }

  abstract class C extends B {
    type U = String
    @notreachable @target("C.foo") override def foo(): U = element.foldRight("")((s, t) => s + t)
  }

  abstract class D extends B {
    type U = Int
    @notreachable @target("D.foo") override def foo(): U = element.length
  }

  abstract class E extends D {
    @target("E.foo") override def foo(): U = element.count((u) => u > 0)
  }
}
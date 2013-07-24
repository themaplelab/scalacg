package tests.tca

import callgraph.annotation.target

object AbstractTypes1 {
  val b1: B = new B {
    type T = List[U]
    type U = String
    @target("B1.foo") override def foo(): U = element.foldRight("")((s, t) => s + t)
    val element = List("pie", "ces o", "f a st", "ring")
  }

  val b2: B = new B {
    type T = List[U]
    type U = Int
    @target("B2.foo") override def foo(): U = element.count((u) => u > 0)
    val element = List(-1, 3, -5, 7, -9)
  }
  
  def main(args: Array[String]) : Unit = {
    new C() { type T = List[String]; val element = List("C") }
	  new D() { type T = List[Int]; val element = List(42) }
	  new E() { type T = List[Int]; val element = List(42) }
	  { "B1.foo"; "B2.foo"; "C.foo"; "D.foo"; "E.foo"; b1 }.foo()
	  
	  { "B1.foo"; "B2.foo"; "C.foo"; "D.foo"; "E.foo"; b2 }.foo()
  }

  abstract class A {
    type T
    val element: T
  }

  abstract class B extends A {
    type U
    type T <: List[U]
    @target("B.foo") def foo(): U
  }

  abstract class C extends B {
    type U = String
    @target("C.foo") override def foo(): U = element.foldRight("")((s, t) => s + t)
  }

  abstract class D extends B {
    type U = Int
    @target("D.foo") override def foo(): U = element.length
  }

  abstract class E extends D {
    @target("E.foo") override def foo(): U = element.count((u) => u > 0)
  }
}
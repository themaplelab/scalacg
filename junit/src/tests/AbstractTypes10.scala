package tests

import callgraph.annotation.target
import callgraph.annotation.reachable
import callgraph.annotation.notreachable

object AbstractTypes10 {
  trait A {
    type T
    val field: T
    @target("A.foo") def foo() = { "C.hashCode"; field }.hashCode()
  }

  class B extends A {
    type T = C
    val field: T = new C()
  }

  class C {
    @target("C.hashCode") override def hashCode() = 42
  }

  
  // FT: inconsistent results? Why does our analysis consider D.hashCode a
  // reachable method, but without making it a target of the call on line 11?
  //
  // KA: this method will be reachable because it could be called back from the library. It has nothing to do with the
  // call on line 11.
  class D {
    @reachable @target("D.hashCode") override def hashCode() = 23
  }
  def main(args: Array[String]): Unit = {
    { "A.foo"; new B()}.foo
    new C()
    new D()
  }
}
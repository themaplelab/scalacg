package tests.tca

import callgraph.annotation.target
import callgraph.annotation.reachable
import callgraph.annotation.notreachable
import callgraph.annotation.invocations

object AbstractTypes10 {
  trait A {
    type T
    val field: T

    @invocations("14: <unannotated> scala.Any: hashCode()", "14: C.hashCode")
    @target("A.foo") def foo() = field.hashCode
    //{ "C.hashCode"; field }.hashCode()
    /*
     * The way we handle library calls now, will make this call resolve to two methods
     * 1) scala.Any: hashCode
     * 2) C.hashCode.
     */
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
    { "A.foo"; new B() }.foo
    new C()
    new D()
  }
}
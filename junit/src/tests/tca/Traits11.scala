package tests.tca

import callgraph.annotation.target

object Traits11 {
  trait A {}
  trait B {
    def foo()
  }
  class C extends B {
    @target("C.foo") def foo() { println("C.foo") }
  }
  class D extends A with B {
    @target("D.foo") def foo() { println("D.foo") }
  }
  @target("bar") def bar(e: A with B) = {
    { "D.foo"; e }.foo()
  }
  def main(args: Array[String]) = {
    { "bar"; this}.bar(new D)
  }
}
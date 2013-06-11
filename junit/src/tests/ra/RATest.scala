package tests.ra

import callgraph.annotation.target

object RATest {
  
  class A {
    
    @target("foo1")
    def foo(i: Int) {}
    
    @target("foo2")
    def foo(i: String) {}
  }
  
  class B {
    @target("foo3")
    def foo() {}
  }

  trait C {
    @target("foo4")
    def foo() {}
  }
  
  def main(args: Array[String]) {
    {"foo1"; "foo2"; "foo3"; "foo4"; new A()}.foo(2)
  }
}
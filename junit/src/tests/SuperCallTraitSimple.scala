package tests

import callgraph.annotation.{reachable, invocations, target}

object SuperCallTraitSimple {

  trait A {
    @target("A.k")
    def k() {
    }
  }

  trait B extends A {
    @invocations("16: A.k")
    def m() {
      super.k()
    }
  }

  class C {}
  
  def main(args: Array[String]) {
    (new C with B).m();
  }
}
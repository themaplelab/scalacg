package tests

import callgraph.annotation.{reachable, invocations, target}

object SuperCallTraitSimple {

  trait A {
    @target("A.m")
    def m() {
    }
  }

  trait B extends A {
    @invocations("15: A.m")
    override def m() {
      super.m()
    }
  }

  class C {}

  def main(args: Array[String]) {
    (new C with B).m()
  }
}
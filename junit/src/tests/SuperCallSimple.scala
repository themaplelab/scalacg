package tests

import callgraph.annotation.{invocations, target, notreachable}

object SuperCallSimple {

  class A {
    @target("A.m")
    def m() {}
  }

  class B extends A {
    @invocations("15: A.m")
    override def m() {
      super.m()
    }
  }

  def main(args: Array[String]) {
    new B().m()
  }
}
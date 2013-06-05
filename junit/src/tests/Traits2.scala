package tests

import callgraph.annotation.target

object Traits2 {
  def main(args: Array[String]) = {

    // test that our algorithm is aware of linearization order
    { "X1.foo"; (new X2 with X1) }.foo();
    { "X2.foo"; (new X1 with X2) }.foo();
  }

  trait Base {
    def foo();
  }

  trait X1 extends Base {
    @target("X1.foo") override def foo(): Unit = { println("X1.foo()"); }
  }

  trait X2 extends Base {
    @target("X2.foo") override def foo(): Unit = { println("X2.foo()"); }
  }
} 
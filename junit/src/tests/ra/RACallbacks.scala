package tests.ra

import callgraph.annotation.target
import callgraph.annotation.reachable

object RACallbacks {
  
  class A {
    @reachable
    override def toString() = "A1";
  }
  
  class B extends A {
//    @reachable
    override def toString() = "B";
  }

  class C {
    @reachable
    override def toString() = "C";
  }
  
  def main(args: Array[String]) {
    var x = List(new A, new A, new A);
    println(x);
  }
}

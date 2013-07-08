package tests

import callgraph.annotation.reachable
import callgraph.annotation.target

object Closures1 {

  def main(args: Array[String]): Unit = {

    // how to specify the expected call graph result for function calls
    // using block syntax such as   { "bar"; bar }(foo1) causes syntax errors..
    { "bar"; this }.bar(foo1);
    { "bar"; this }.bar(foo2);

    { "baz"; this }.baz(zap1);
    { "baz"; this }.baz(zap2);
  }

  /*
   * Karim: the following methods are reachable because they will appear in the apply method of the Function0/1 objects
   * that will be created after desugaring closures.
   */
  @reachable def foo1(): Unit = println("foo1")
  @reachable def foo2(): Unit = println("foo2")
  @reachable def zap1(i: Int): Unit = println("zap1")
  @reachable def zap2(i: Int): Unit = println("zap2")

  @reachable
  @target("bar")
  def bar(z: () => Unit): Unit = { z(); }

  @reachable
  @target("baz")
  def baz(z: (Int) => Unit): Unit = { z(3); }

}
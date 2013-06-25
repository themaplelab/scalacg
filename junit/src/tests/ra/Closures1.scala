package tests.ra

import callgraph.annotation.target

object Closures1 {

  def main(args: Array[String]): Unit = {

    // how to specify the expected call graph result for function calls
    // using block syntax such as   { "bar"; bar }(foo1) causes syntax errors..
    { "bar"; this}.bar(foo1);
    { "bar"; this}.bar(foo2);

    { "baz"; this}.baz(zap1);
    { "baz"; this}.baz(zap2);
  }

  @target("foo1") def foo1(): Unit = { println("foo1"); }
  @target("foo2") def foo2(): Unit = { println("foo2"); }

  @target("zap1") def zap1(i: Int): Unit = { println("zap1"); }
  @target("zap2") def zap2(i: Int): Unit = { println("zap2"); }

  @target("bar") def bar(z: () => Unit): Unit = { z(); }
  @target("baz") def baz(z: (Int) => Unit): Unit = { z(3); }

}
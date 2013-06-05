package tests

import callgraph.annotation.target

object Closures2 {

  def main(args: Array[String]): Unit = {

    def main(args: Array[String]): Unit = {

      fun(main _); // pass main as argument

      bar(foo1);
      bar(foo2);

    }
  }

  @target("fun") def fun(z: Any): Unit = {}

  @target("foo1") def foo1(): A = { return null; }

  @target("foo2") def foo2(): B = { return null; }

  @target("bar") def bar(z: () => A) {
    z();
  }

  class A {}
  class B extends A {}

}
package tests

import callgraph.annotation.target

object Closures2 {

  def main(args: Array[String]): Unit = {

      { "fun"; this}.fun(main _); // pass main as argument

      { "bar"; this}.bar(foo1);
      { "bar"; this}.bar(foo2);

      { "FORCE_TEST_FAILURE"; this}.fail(); // force test to fail until we have an @invocations assertion on the calls in bar
  }
  
  def fail(){}

  @target("fun") def fun(z: Any): Unit = {}

  @target("foo1") def foo1(): A = { return null; }

  @target("foo2") def foo2(): B = { return null; }

  @target("bar") def bar(z: () => A) {
    z();
  }

  class A {}
  class B extends A {}

}
package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable
import ca.uwaterloo.scalacg.annotation.target

object Closures2 {

  @reachable
  def main(args: Array[String]): Unit = {

    { "fun"; this }.fun(main _); // pass main as argument

    { "bar"; this }.bar(foo1);
    { "bar"; this }.bar(foo2);
  }

  def fail() {}

  @target("fun") def fun(z: Any): Unit = {}

  @reachable
  @target("foo1")
  def foo1(): A = { return null; }

  @reachable
  @target("foo2")
  def foo2(): B = { return null; }

  @reachable
  @target("bar")
  def bar(z: () => A) {
    z();
  }

  class A {}
  class B extends A {}

}
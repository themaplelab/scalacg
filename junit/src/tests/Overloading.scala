package tests

/**
 * This test shows if we correctly capture overloaded methods.
 */
object Overloading {

  var a = new A()

  { "A.foo(Int)"; a }.foo(1);
  { "A.foo(String)"; a }.foo("bla");
  { "A.foo(Int, Int)"; a }.foo(1, 1);
  { "A.foo(Array[Int])"; a }.foo(Array[Int](1, 1, 1));

  class A {
    @target("A.foo(Int)") def foo(x: Int): Unit = {}
    @target("A.foo(String)") def foo(x: String): Unit = {}
    @target("A.foo(Int, Int)") def foo(x: Int, y: Int): Unit = {}
    @target("A.foo(Array[Int])") def foo(x: Array[Int]): Unit = {}
  }
}
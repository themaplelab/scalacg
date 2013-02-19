package tests

object Test2 {

  var a = new A();

  { "A.foo"; a }.foo();

  class A {
    @target("A.foo") def foo(): Unit = {}
  }

}
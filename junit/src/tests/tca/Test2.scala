package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object Test2 {

  var a = new A();

  def main(args: Array[String]) = {
    { "A.foo"; a }.foo();
  }

  class A {
    @target("A.foo") def foo(): Unit = {}
  }

}
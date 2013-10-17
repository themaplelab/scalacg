package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object SuperCall1 {
//
  trait X {
    @target("X.bar")
    def bar() { println("X.bar"); }
  }

  trait Y extends X {
    @target("Y.foo")
    @invocations("17: X.bar", "17: Z.bar")
    def foo() {
      super.bar()
    }
  }

  trait Z extends X {
    @target("Z.bar") override def bar() { println("Z.bar"); }
  }
  
  def main(args: Array[String]) {
    {"Y.foo"; (new Y with Z)}.foo(); // calls X.bar
    {"Y.foo"; (new Z with Y)}.foo(); // calls Z.bar
  }
}
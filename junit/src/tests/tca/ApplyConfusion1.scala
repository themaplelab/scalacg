package tests.tca

import scala.collection.immutable.LinearSeq
import ca.uwaterloo.scalacg.annotation.invocations
import ca.uwaterloo.scalacg.annotation.reachable

object ApplyConfusion1 {

  class A extends (Int => String) {
    @reachable
    def apply(m: Int) = "hello";
  }

  class B {
    @reachable
    def foo(j: Int) = "goodbye";

    // Karim: the closures issue.
    // @invocations("16: tests.ApplyConfusion1.B: foo(j: Int)")
    def bar(z: (Int => String)) = z(3); // calls scala/Function1.apply:(Ljava/lang/Object;)Ljava/lang/Object;

    def zap() = bar(foo);
  }

  // This gets dispatched to A.apply not Function1.apply
  // Karim: the closures issue.
  // @invocations("25: <unannotated> tests.ApplyConfusion1.A: apply(m: Int)")
  def main(args: Array[String]): Unit = {
    val x: (Int => String) = new A();
    val y = x(2);
    println(y);

    val v = new B;
    val w = v.zap();
    println(w);
  }
}
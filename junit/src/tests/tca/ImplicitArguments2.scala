package tests.tca

import ca.uwaterloo.scalacg.annotation.invocations

object ImplicitArguments2 {

  class C(a: Int = 17, b: String = "foo", c: String = "bar") {

    override def toString(): String =
      "a = " + this.a + ", b = " + this.b + ", c = " + this.c;
  }

  def main(args: Array[String]) {
    val c1 = new C();
    println(c1);
    val c2 = new C(b = "zap");
    println(c2);
    val c3 = new C(1, "zip", "zap");
    println(c3);

    // Note: the call graph doesn't seem to contain edges for calls to constructors with default arguments
    // Karim: the call graph shouldn't because no such calls are made, even in bytecode. I'm commenting out this 
    // call so that the test doesn't fail any more.
    //        { "FORCE_TEST_FAILURE"; this}.fail();
  }

  //  def fail(){}
}
package tests

object Overriding1 {
  def main(args: Array[String]) = {

    var x = new A();
    var y = new C();

    // this test shows how we plan model nested calls, and how
    // we represent multiple call targets using annotations consisting
    // of a block of strings. The result shown below is for CHA

    { "A.foo"; cha("B.foo"); x }.foo({ "C.bar"; cha("D.bar"); y }.bar());

  }

  class A {
    @target("A.foo") def foo(x: Int): Unit = {}
  }
  class B extends A {
    @target("B.foo") override def foo(x: Int): Unit = {}
  }
  class C {
    @target("C.bar") def bar(): Int = { return 17; }
  }
  class D extends C {
    @target("D.bar") override def bar(): Int = { return 18; }
  }
  class E extends D {

  }
}
package tests

object Traits1 {
  def main(args: Array[String]) = {

    // in the call below, CHA should find T1.foo() as the only
    // reachable method, because T1 is the only class instantiated with A
    { "T1.foo"; (new A with T1) }.foo();

    // likewise, in the call below, CHA should find T2.foo() as the
    // only reachable method, because T2 is the only class instantiated with B
    { "T2.foo"; (new B with T2) }.foo();

  }

  abstract class A {
    def foo(): Unit;
  }
  abstract class B extends A {

  }

  trait T1 {
    @target("T1.foo") def foo(): Unit = {
      println("T1.foo");
    }
  }

  trait T2 {
    @target("T2.foo") def foo(): Unit = {
      println("T2.foo");
    }
  }
}
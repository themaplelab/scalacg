package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object VariantsExample {
  class A {
    @target("A.foo") def foo = "A.foo"
  }
  class B extends A {
    @target("B.foo") override def foo = "B.foo"
  }
  class C {
    @target("C.foo") def foo = "C.foo"
  }
  class D {
    @target("D.foo") def foo = "D.foo"
  }
  class CallSiteClass[T <: A](val receiver: T) {
    def callsite = {
      /*
       * resolves to:
       * B.foo with TCA
       * B.foo, A.foo with BA
       * B.foo, A.foo, C.foo with TCRA
       * B.foo, A.foo, C.foo, D.foo with RA
       */
      { "B.foo"; receiver }.foo
    }
  }
  def main(args: Array[String]): Unit = {
    new A
    val receiver = new B
    new C
    val callSiteClass = new CallSiteClass[B](receiver);
    callSiteClass.callsite
  }
}
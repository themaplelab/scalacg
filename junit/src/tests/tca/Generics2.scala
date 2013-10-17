package tests.tca

import ca.uwaterloo.scalacg.annotation.invocations
import ca.uwaterloo.scalacg.annotation.target

object Generics2 {

  trait A[T] {
    var field: T

    @target("A.foo") def foo() = { "C.hashCode"; field }.hashCode()
  }

  // See my comment below in the main method
  //  @invocations("16: field_=")
  @invocations("18: <unannotated> tests.tca.Generics2.C: <init>()")
  @target("Generics2.foo") def foo(ac: A[C]) {
    ac.field = new C()
  }

  class C {
    @target("C.hashCode") override def hashCode() = 42
  }

  class D {
    @target("D.hashCode") override def hashCode() = 23
  }

  def main(args: Array[String]) {
    /*
     * KA: That's not the correct way of overriding field accessors. In fact, this anonymous class gets compile into 
     * a totally different code than what's expected.
     * 	  private[this] var field: Null = null;
     *    <accessor> def field: Null = $anon.this.field;
     *    <accessor> def field_=(x$1: Null): Unit = $anon.this.field = x$1;
     *    def field_=(c: tests.Generics2.C): Unit = $anon.this.field_=(c)
     *
     * Since Null is the bottom of all Scala reference types, when there's a call in foo to field_=, the staticTarget
     * never matches this method because nothing will be a subtype of Null. It's very weird, because I thought field
     * will get the type C as expected from the declaration of trait A.  
     */
    val a = new A[C] {
      var field = null

      def field_=(c: C) {
        field = c
      }
    }
    { "Generics2.foo"; this }.foo(a)
  }
}
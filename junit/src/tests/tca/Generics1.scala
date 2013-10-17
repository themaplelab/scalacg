package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.reachable
import ca.uwaterloo.scalacg.annotation.notreachable

object Generics1 {
  class C {
    @target("C.foo") def foo() = "C"
  }
  
  class D extends C{
    @notreachable @target("D.foo") override def foo() = "D"
  }
  
  class A[T <: C](var elem:T) {
    @target("A.bar") def bar() = { "C.foo"; elem }.foo
  }
  
  def main(args: Array[String]) : Unit = {
    { "A.bar"; new A[C](new C)}.bar()
  }
}
package tests

object Generics1 {
  class C {
    @target("C.foo") def foo() = "C"
  }
  
  class D extends C{
    @target("D.foo") override def foo() = "D"
  }
  
  class A[T <: C](var elem:T) {
    def foo() = { "C.foo"; "D.foo"; elem }.foo
  }
  
  // add another test with D
  new A[C](new C).foo()
}
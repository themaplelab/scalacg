package tests

object Traits11 {
  trait A {}
  trait B {
    def foo()
  }
  class C extends B {
    @target("C.foo") def foo() { println("C.foo") }
  }
  class D extends A with B {
    @target("D.foo") def foo() { println("D.foo") }
  }
  def callSite(receiver: A with B) = {
    { "D.foo"; receiver }.foo()
  }
  def main(args: Array[String]) = {
    callSite(new D)
  }
}
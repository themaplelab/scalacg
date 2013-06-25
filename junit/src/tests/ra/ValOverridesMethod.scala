package tests.ra

object ValOverridesMethod {
  abstract class A {
    def m: Int
  }

  class B extends A {
    val m = 5
  }
  
  def main(args: Array[String]) = {
    val a: A = new B
    println(a.m)
  }
}

package tests.matching

import tests.target

object CaseClass {

  /**
   * Testing pattern matching on case class
   */
  def main(args: Array[String]) {
    val e: Expr = Lit(value = true)
    e match {
      case Lit(v)    => {"foo"; this}.foo()
      case Var(z)    => Console.println(z)
      case And(l, r) => Console.println(l + ", " + r)
    }
  }

  @target("foo") def foo() {
    Console.println("foo")
  }
}

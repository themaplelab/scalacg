package tests.matching

import callgraph.annotation.target

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
  
  abstract class Expr
	case class Lit(value: Boolean) extends Expr
	case class Var(name: String) extends Expr
	case class And(left: Expr, right: Expr) extends Expr
}

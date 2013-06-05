package tests.matching

import callgraph.annotation.target

object CaseClassWildcard {

  /**
   * Testing pattern matching on case class: wildcard matching
   */
  def main(args: Array[String]) {
    val e: Expr = Lit(value = true)
    e match {
      case Var(z) => Console.println(z)
      case _      => {"foo"; this}.foo()
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

package tests.matching

import callgraph.annotation.target

object MatchExpression {

  /**
   * Testing call in match expression
   */
  def main(args: Array[String]) {
    val e: Expr = Lit(true)
    ({"inverse"; this}.inverse(e)) match {
      case Lit(v)    => Console.println(v)
      case Var(z)    => Console.println(z)
      case And(l, r) => Console.println(l + ", " + r)
    }
  }

  @target("inverse") def inverse(e: Expr) = {
    e match {
      case Lit(v) => Lit(false)
      case _      => Var("v")
    }
  }
  
  abstract class Expr
	case class Lit(value: Boolean) extends Expr
	case class Var(name: String) extends Expr
	case class And(left: Expr, right: Expr) extends Expr
}

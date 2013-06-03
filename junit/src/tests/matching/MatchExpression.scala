package tests.matching

import tests.target

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
}

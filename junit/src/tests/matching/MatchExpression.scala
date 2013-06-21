package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object MatchExpression {

  /**
   * Testing call in match expression
   */
  
  @invocations("26: <unannotated> tests.matching.MatchExpression.Lit: <init>(value: Boolean)",
               "27: <unannotated> scala.MatchError: <init>(obj: Any)",
               "28: <unannotated> scala.Any: asInstanceOf()",
               "28: <unannotated> scala.Any: isInstanceOf()",
               "28: <unannotated> tests.matching.MatchExpression.Lit: value()",
               "29: <unannotated> scala.Any: asInstanceOf()",
               "29: <unannotated> scala.Any: isInstanceOf()",
               "29: <unannotated> tests.matching.MatchExpression.Var: name()",
               "30: <unannotated> scala.Any: asInstanceOf()", 
               "30: <unannotated> scala.Any: isInstanceOf()",
               "30: <unannotated> tests.matching.MatchExpression.And: left()",
               "30: <unannotated> tests.matching.MatchExpression.And: right()"
               )
  def main(args: Array[String]) {
    val e: Expr = Lit(true)
    ({"inverse"; this}.inverse(e)) match {
      case Lit(v)    => Console.println(v)
      case Var(z)    => Console.println(z)
      case And(l, r) => Console.println(l + ", " + r)
    }
  }

  @invocations("40: <unannotated> scala.Any: isInstanceOf()",
               "40: <unannotated> tests.matching.MatchExpression.Lit: <init>(value: Boolean)",
               "41: <unannotated> tests.matching.MatchExpression.Var: <init>(name: String)")
  @target("inverse") 
  def inverse(e: Expr) = {
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

package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object MatchExpression {

  /**
   * Testing call in match expression
   */
  
  @invocations("29: <unannotated> tests.matching.MatchExpression.Lit: <init>(value: Boolean)", 
               "30: <unannotated> scala.MatchError: <init>(obj: Any)", 
               // "31: <unannotated> java.lang.Object: ne(x$1: AnyRef)", see CaseClass 
               "31: <unannotated> scala.Any: asInstanceOf()", 
               "31: <unannotated> scala.Any: isInstanceOf()", 
               "31: <unannotated> tests.matching.MatchExpression.Lit: value()", 
               // "32: <unannotated> java.lang.Object: ne(x$1: AnyRef)", 
               "32: <unannotated> scala.Any: asInstanceOf()", 
               "32: <unannotated> scala.Any: isInstanceOf()", 
               "32: <unannotated> tests.matching.MatchExpression.Var: name()", 
               // "33: <unannotated> java.lang.Object: ne(x$1: AnyRef)", 
               "33: <unannotated> scala.Any: asInstanceOf()", 
               "33: <unannotated> scala.Any: isInstanceOf()", 
               "33: <unannotated> tests.matching.MatchExpression.And: left()",
               "33: <unannotated> tests.matching.MatchExpression.And: right()"
               )
  def main(args: Array[String]) {
    val e: Expr = Lit(true)
    ({"inverse"; this}.inverse(e)) match {
      case Lit(v)    => Console.println(v)
      case Var(z)    => Console.println(z)
      case And(l, r) => Console.println(l + ", " + r)
    }
  }

  @invocations(// "46: <unannotated> java.lang.Object: ne(x$1: AnyRef)", see CaseClass 
               // "46: <unannotated> scala.Any: asInstanceOf()", see CaseClass
               // "46: <unannotated> tests.matching.MatchExpression.Lit: value()", see CaseClass
               "46: <unannotated> scala.Any: isInstanceOf()", 
               "46: <unannotated> tests.matching.MatchExpression.Lit: <init>(value: Boolean)",  
               "47: <unannotated> tests.matching.MatchExpression.Var: <init>(name: String)")
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

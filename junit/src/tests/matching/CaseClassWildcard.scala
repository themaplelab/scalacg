package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object CaseClassWildcard {

  /**
   * Testing pattern matching on case class: wildcard matching
   */
  @invocations("18: <unannotated> tests.matching.CaseClassWildcard.Lit: <init>(value: Boolean)", 
               // "20: <unannotated> java.lang.Object: ne(x$1: AnyRef)", this call will not be captured, see CaseClass 
               "20: <unannotated> scala.Any: asInstanceOf()", 
               "20: <unannotated> scala.Any: isInstanceOf()", 
               "20: <unannotated> tests.matching.CaseClassWildcard.Var: name()"
               )
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

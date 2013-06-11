package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object CaseClass {

  /**
   * Testing pattern matching on case class
   */
  @invocations("28: <unannotated> tests.matching.CaseClass.Lit: <init>(value: Boolean)", 
               "29: <unannotated> scala.MatchError: <init>(obj: Any)", 
               "30: <unannotated> java.lang.Object: ne(x$1: AnyRef)", 
               "30: <unannotated> scala.Any: asInstanceOf()", 
               "30: <unannotated> scala.Any: isInstanceOf()", 
               "30: <unannotated> tests.matching.CaseClass.Lit: value()",
               "31: <unannotated> java.lang.Object: ne(x$1: AnyRef)", 
               "31: <unannotated> scala.Any: asInstanceOf()", 
               "31: <unannotated> scala.Any: isInstanceOf()", 
               "31: <unannotated> tests.matching.CaseClass.Var: name()", 
               "32: <unannotated> java.lang.Object: ne(x$1: AnyRef)", 
               "32: <unannotated> scala.Any: asInstanceOf()", 
               "32: <unannotated> scala.Any: isInstanceOf()", 
               "32: <unannotated> tests.matching.CaseClass.And: left()", 
               "32: <unannotated> tests.matching.CaseClass.And: right()"
               )
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

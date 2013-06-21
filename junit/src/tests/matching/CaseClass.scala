package tests.matching

import callgraph.annotation.invocations
import callgraph.annotation.target

object CaseClass {

  /**
   * Testing pattern matching on case class
   */
  @invocations("24: <unannotated> tests.matching.CaseClass.Lit: <init>(value: Boolean)",
    "25: <unannotated> scala.MatchError: <init>(obj: Any)",
    "26: <unannotated> scala.Any: isInstanceOf()",
    "26: <unannotated> scala.Any: asInstanceOf()", // These two calls are in not in the call graph because there's no
    "26: <unannotated> tests.matching.CaseClass.Lit: value()", // call site for Lit.value in main, though there's one in the intermediate code! Weird.
    "27: <unannotated> scala.Any: asInstanceOf()",
    "27: <unannotated> scala.Any: isInstanceOf()",
    "27: <unannotated> tests.matching.CaseClass.Var: name()",
    "28: <unannotated> scala.Any: asInstanceOf()",
    "28: <unannotated> scala.Any: isInstanceOf()",
    "28: <unannotated> tests.matching.CaseClass.And: left()",
    "28: <unannotated> tests.matching.CaseClass.And: right()")
  def main(args: Array[String]) {
    val e: Expr = Lit(value = true)
    e match {
      case Lit(v) => { "foo"; this }.foo(v)
      case Var(z) => Console.println(z)
      case And(l, r) => Console.println(l + ", " + r)
    }
  }

  @target("foo") def foo(v: Boolean) {
    Console.println("foo")
  }

  abstract class Expr
  case class Lit(value: Boolean) extends Expr
  case class Var(name: String) extends Expr
  case class And(left: Expr, right: Expr) extends Expr
}

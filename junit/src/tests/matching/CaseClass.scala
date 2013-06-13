package tests.matching

import callgraph.annotation.invocations
import callgraph.annotation.target

object CaseClass {

  /**
   * Testing pattern matching on case class
   */
  @invocations("27: <unannotated> tests.matching.CaseClass.Lit: <init>(value: Boolean)",
    "28: <unannotated> scala.MatchError: <init>(obj: Any)",
    //"29: <unannotated> java.lang.Object: ne(x$1: AnyRef)", // those won't appear b/c java.lang.Object is not instantiated in the application.
    "29: <unannotated> scala.Any: isInstanceOf()",
    /*"29: <unannotated> scala.Any: asInstanceOf()", // These two calls are in not in the call graph because there's no
    "29: <unannotated> tests.matching.CaseClass.Lit: value()",*/ // call site for Lit.value in main, though there's one in the intermediate code! Weird.
    //"30: <unannotated> java.lang.Object: ne(x$1: AnyRef)",
    "30: <unannotated> scala.Any: asInstanceOf()",
    "30: <unannotated> scala.Any: isInstanceOf()",
    "30: <unannotated> tests.matching.CaseClass.Var: name()",
    //"31: <unannotated> java.lang.Object: ne(x$1: AnyRef)",
    "31: <unannotated> scala.Any: asInstanceOf()",
    "31: <unannotated> scala.Any: isInstanceOf()",
    "31: <unannotated> tests.matching.CaseClass.And: left()",
    "31: <unannotated> tests.matching.CaseClass.And: right()")
  def main(args: Array[String]) {
    val e: Expr = Lit(value = true)
    e match {
      case Lit(v) => { "foo"; this }.foo()
      case Var(z) => Console.println(z)
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

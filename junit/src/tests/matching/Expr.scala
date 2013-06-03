package tests.matching

abstract class Expr
case class Lit(value: Boolean) extends Expr
case class Var(name: String) extends Expr
case class And(left: Expr, right: Expr) extends Expr
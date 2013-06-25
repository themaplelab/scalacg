package tests.ra

import callgraph.annotation.target

object CaseClass1 {

   abstract class Expr
    case class Var(name: String) extends Expr
    case class Number(num: Double) extends Expr
    case class UnOp(operator: String, arg: Expr) extends Expr
    case class BinOp(operator: String, 
        left: Expr, right: Expr) extends Expr
  
  
  def main(args: Array[String]): Unit = {
     val e1 = Var("zz");
     val e2 = BinOp("+", e1, e1);
     val e3 = UnOp("-", e2);
     println({ "foo"; this}.foo(e3)); // prints "-(zz+zz)"
   }
   
   @target("foo") def foo(e : Expr) : String = e match {
     case UnOp("-", e) => return "-" + "(" + { "foo"; this}.foo(e) + ")";
     case BinOp("+", e1, e2) => return { "foo"; this}.foo(e1) + "+" + { "foo"; this}.foo(e2);
     case Var(x) => return x;
   }
  
}
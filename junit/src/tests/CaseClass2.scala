package tests

object CaseClass2 {

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
     println(foo(e3)); // prints "-(zz+zz)"
     val e4 = Number(3.14);
     try {
    	 { "foo"; this}.foo(e4);
     } catch {
       case e:MatchError => println("exception")
     }
   }
   
   @target("foo") def foo(e : Expr) : String = e match {
     case UnOp("-", e) => return "-" + "(" + { "foo"; this}.foo(e) + ")";
     case BinOp("+", e1, e2) => return foo(e1) + "+" + { "foo"; this}.foo(e2);
     case Var(x) => return x;
   }
  
}
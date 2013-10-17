package tests.tca.matching

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object CaseClassInstanceof {

   /**
    * Testing extractors
    */
   @invocations("15: <unannotated> scala.Any: isInstanceOf([T0])")
   def main(args: Array[String]) {
    val e: Expr = Lit(value = true)
    e match {
      case Lit(v)    => println("right")
      case _         => println("wrong")
    }
  }
   
  abstract class Expr
	case class Lit(value: Boolean) extends Expr
	case class Var(name: String) extends Expr
	case class And(left: Expr, right: Expr) extends Expr
 }

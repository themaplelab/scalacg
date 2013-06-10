package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object Operator1 {

  class C(x : Int) {    
    @target("::") def :: (y : Int): C = new C(x * y)
    override def toString() = x.toString;
  }
  
  @invocations("ADD ASSERTION FOR THE CALL ON LINE 15")
  def main(args: Array[String]): Unit = {
     val c = new C(10);
     val d = 10 :: c; // how to place an assertion here??
     val e =  { "::"; d}.::(10);
     println(e);
  }
}
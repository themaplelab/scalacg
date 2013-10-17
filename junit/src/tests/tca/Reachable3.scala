package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable
import scala.collection.mutable.ListBuffer
import ca.uwaterloo.scalacg.annotation.notreachable
import ca.uwaterloo.scalacg.annotation.invocations

object Reachable3 {

  @invocations("16: <unannotated> tests.tca.Reachable3.A: <init>(x: String)",
               "17: <unannotated> tests.tca.Reachable3.A: <init>(x: String)",
               "18: <unannotated> scala.collection.mutable.ListBuffer: <init>()"
              )
  def main(args: Array[String]) {
    
    val a1 = new A("foo")
    val a2 = new A("bar") 
    val x = new ListBuffer[A]()
    x += a1
    x += a2
    println(x)
  }
 
  class A(x : String) { 
    @reachable
    override def toString = { x + x; }
    
    @notreachable
    def bar(){}
  }

}
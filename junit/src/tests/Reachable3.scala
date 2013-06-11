package tests

import callgraph.annotation.reachable
import scala.collection.mutable.ListBuffer
import callgraph.annotation.notreachable
import callgraph.annotation.invocations

object Reachable3 {

  @invocations("16: <unannotated> tests.Reachable3.A: <init>(x: String)", 
               "17: <unannotated> tests.Reachable3.A: <init>(x: String)"
//               "18: <unannotated> scala.collection.mutable.ListBuffer: <init>()"
              )
  def main(args: Array[String]): Unit = {
    
    val a1 = new A("foo")
    val a2 = new A("bar") 
    val x = new ListBuffer[A]()
    x += a1
    x += a2
    println(x);

     { "FORCE_TEST_FAILURE"; this}.fail();
  }
  
  def fail(){}
 
  class A(x : String) { 
    @reachable
    override def toString() = { x + x; }
    
    @notreachable
    def bar(){}
  }

}
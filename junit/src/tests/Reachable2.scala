package tests

import callgraph.annotation.reachable
import scala.collection.mutable.ListBuffer
import callgraph.annotation.invocations

object Reachable2 {

  @invocations("15: <unannotated> tests.Reachable2.A: <init>(x: String)", 
               "16: <unannotated> tests.Reachable2.A: <init>(x: String)",
               "17: <unannotated> scala.collection.mutable.ListBuffer: <init>()"
              )
  def main(args: Array[String]): Unit = {
    
    val a1 = new A("foo")
    val a2 = new A("bar") 
    val x = new ListBuffer[A]()
    x += a1
    x += a2
    println(x);
  }
 
  class A(x : String) { 
    @reachable
    override def toString() = { x + x; }
  }

}
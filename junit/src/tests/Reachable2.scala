package tests

import callgraph.annotation.reachable
import scala.collection.mutable.ListBuffer

object Reachable2 {

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
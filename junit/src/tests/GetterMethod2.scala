package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object GetterMethod2 {

  
  class A(x : String){ 
    
    val y = "hi"
    
    
    @invocations("17: <unannotated> tests.GetterMethod2.A: y()")
    @target("A.foo") def foo() = { 
      val p = this.x; 
      this.y 
    }
    
  }
  
  def main(args: Array[String]): Unit = {
    val a = new A("hello");
     
    {"A.foo"; a}.foo();
  }

}
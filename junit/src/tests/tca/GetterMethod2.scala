package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object GetterMethod2 {

  
  class A(x : String){ 
    
    val y = "hi"
    
    
    @invocations("17: <unannotated> tests.tca.GetterMethod2.A: y()")
    @target("A.foo") def foo() = { 
      val p = this.x
      this.y 
    }
    
  }
  
  def main(args: Array[String]) {
    val a = new A("hello")

    {"A.foo"; a}.foo()
  }

}
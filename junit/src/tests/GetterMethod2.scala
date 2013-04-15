package tests

object GetterMethod2 {

  
  class A(x : String){ 
    
    @target("y") val y = "hi"
    
    // there is a call to a getter method for y, but not for x. How to assert this?
    def foo() = { val p = this.x; { "y"; this}.y }
    
  }
  
  def main(args: Array[String]): Unit = {
    val a = new A("hello");
     
    a.foo();
     
  }

}
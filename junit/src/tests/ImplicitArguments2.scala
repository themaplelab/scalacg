package tests

 
object ImplicitArguments2 {
   
  class C(a : Int=17, b : String = "foo", c : String="bar"){
    
    override def toString() : String = 
       "a = " + this.a + ", b = " + this.b + ", c = " + this.c; 
  }
  
  
  def main(args: Array[String]) {
        val c1 = new C();
        println(c1);
        val c2 = new C(b="zap");
        println(c2);
        val c3 = new C(1, "zip", "zap");
        println(c3);
        
        // add @invocations for the various constructor calls
        { "FORCE_TEST_FAILURE"; this}.fail();
  }
  
  def fail(){}
}
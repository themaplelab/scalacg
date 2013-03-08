package tests

object SuperCall {

   trait X {
       @target("X.bar") def bar(){ println("X.bar"); }
    }
  
	trait Y extends X {
	   @target("Y.foo") def foo() = { super.bar(); }  // { "X.bar"; "Z.bar"; super }.bar(); is not legal Scala code
	}
	
	trait Z extends X {
	  @target("Z.bar") override def bar(){  println("Z.bar"); }
	} 
  
  def main(args: Array[String]): Unit = {
      { "Y.foo"; (new Y with Z)}.foo(); // calls X.bar
	  { "Y.foo"; (new Z with Y)}.foo(); // calls Z.bar
  }

}
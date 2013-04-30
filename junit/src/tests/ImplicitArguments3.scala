package tests

object ImplicitArguments3 {

  sealed case class C (
    val a : String = "foo",
    val b : Int = 17,
    val c : Any = "bar"
  )
  
  @target("foo") def foo(p : C) = {
    p match {
      case C(a,b,c) => println(a + "," + b + "," + c);
    }
  }
  
  def main(args: Array[String]): Unit = {
    val x = new C(c = "zap"); // TODO: add assertion on constructor call
    { "foo"; this}.foo(x);

    val y = new C(); // TODO: add assertion on constructor call
    { "foo"; this}.foo(y);
    
     { "FORCE_TEST_FAILURE"; this}.fail();
  }
  
  def fail(){}


  
}
package tests

object Operator1 {

  class C(x : Int) {    
    def :: (y : Int): C = new C(x * y)
    @target("::") override def toString() = x.toString;
  }
  
  def main(args: Array[String]): Unit = {
     val c = new C(10);
     val d = 10 :: c; // how to place an assertion here??
     val e =  { "::"; d}.::(10);
     println(e);
  }

}
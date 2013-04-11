package tests

object Implicits1 {
  class B(n : Int){
    def get : Int = n;
    override def toString() = "B[" + n + "]";
  } 
  class A(p : Int) {
    override def toString() = "A[" + p + "]";
  }
  @target("B2A") implicit def B2A(b : B):A = new A(b.get*b.get);

  def printA(a : A){ 
    println(a)
  }
  
  def main(args: Array[String]) {
     val b1 = new B(7);
     printA(b1); // prints "A[49]"
  }
}
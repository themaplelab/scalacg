package tests.tca

object Apply3 {

  def main(args: Array[String]) = {
    val values = List[Int](1, 2, 3, 4, 5)
    val x = new C();
    val y = x(1); // calls apply(Int)
    println(y);
    val z = x("hello"); // // calls apply(String)
    println(z);
    x(); // calls apply()
  }

  class C {
    def apply(x: Int) = x + 5
    def apply(y : String) = y + y
    def apply() = { println("foo") }
  }

}
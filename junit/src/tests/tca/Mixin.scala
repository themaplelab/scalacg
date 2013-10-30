package tests.tca

object Mixin {

  def main(args: Array[String]) = {
    val xz = new X with Z
  }
  
  trait X 
  trait Y extends X {
    val y = List("y")
  }
  trait Z
  abstract class C extends X

}
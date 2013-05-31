package tests

object Apply1 {

  def main(args: Array[String]) = {
    val v = AddFive()(1)
    println(v)
  }

  case class AddFive {
    def apply(x: Int) = x + 5
  }

}
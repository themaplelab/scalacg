package tests

object Apply2 {

  def main(args: Array[String]) = {
    val values = List[Int](1, 2, 3, 4, 5)
    for(v <- values) {
      println(AddFive()(v))
    }
  }

  case class AddFive {
    def apply(x: Int) = x + 5
  }

}
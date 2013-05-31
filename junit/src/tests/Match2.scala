package tests

object Match2 {

  def main(args: Array[String]) = {
    val e = X(1)
    e match {
      case X(1) =>
        println("match")
      case _ =>
        println("no match")
    }
  }

  case class X(a: Int) {

  }

}
package tests

object LiftedMethod1 {

  def main(args: Array[String]) = {
    foo
  }

  def foo = {
    def toText(i: Int) = empty + i
    def empty = ""
    val v = toText(1)
  }

}
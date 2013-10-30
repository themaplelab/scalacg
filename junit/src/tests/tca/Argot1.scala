package tests.tca

object Argot1 {

  def main(args: Array[String]) = {
    val p = new { val valueName = "foo" } with Parameter[Int]
  }

  trait HasValue[T] {
    val valueName: String
  }
  
  trait Bla

  trait Parameter[T] extends Bla with HasValue[T] {
    require(valueName.length > 0)
  }
}
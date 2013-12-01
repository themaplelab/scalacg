package tests.tca.wala

object Equals {

  def main(args: Array[String]): Unit = {
    val a = new A
    a == ""
  }

  class A {
    override def equals(that: Any) = true
  }
}
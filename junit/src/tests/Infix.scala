package tests

/**
 * A test case that represents the use of infix notation in the phantm benchmark.
 */
object Infix {

  def main(args: Array[String]) = {
    val sum = {"One.+";One} + {"Number.+";Two} + Three + Four
    println(sum.v)
  }

  object One extends Number(1) {
    @target("One.+") override def +(n: Number) : Number = {
      new Number(v + 10)
    }
  }
  object Two extends Number(2) {
  }
  object Three extends Number(3) {
  }
  object Four extends Number(4) {
  }

  class Number(val v: Int) {
    @target("Number.+") def +(n: Number): Number = {
      new Number(v + n.v)
    }
  }
}
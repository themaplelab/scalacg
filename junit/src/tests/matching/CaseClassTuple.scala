package tests.matching

import tests.target

object CaseClassTuple {

  /**
   * Testing pattern matching on standard case class 'Tuple'
   */
  def main(args: Array[String]) {
    val tuple = (1, 2, 3)
    tuple match {
      case (a, b, 4) => Console.println("wrong")
      case (a, b, c) => {"foo"; this}.foo()
    }
  }

  @target("foo") def foo() {
    Console.println("foo")
  }
}

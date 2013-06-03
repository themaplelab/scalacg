package tests.matching

import tests.target

object CaseClassOption {

  /**
   * Testing pattern matching on standard case class 'Option'
   */
  def main(args: Array[String]) {
    val some: Option[Integer] = Some(1)  // todo: too simple?
    some match {
      case Some(a) => {"foo"; this}.foo()
      case None    => Console.println("wrong")
    }
  }

  @target("foo") def foo() {
    Console.println("foo")
  }
}

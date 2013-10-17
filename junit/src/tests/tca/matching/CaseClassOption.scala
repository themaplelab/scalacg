package tests.tca.matching

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object CaseClassOption {

  /**
   * Testing pattern matching on standard case class 'Option'
   */
  @invocations("13: <unannotated> scala.Some: <init>(x: A)", "14: <unannotated> scala.MatchError: <init>(obj: Any)")
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

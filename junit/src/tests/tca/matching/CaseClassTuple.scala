package tests.tca.matching

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations

object CaseClassTuple {

  /**
   * Testing pattern matching on standard case class 'Tuple'
   */
  @invocations("13: <unannotated> scala.Tuple3: <init>(_1: T1,_2: T2,_3: T3)", "14: <unannotated> scala.MatchError: <init>(obj: Any)")
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

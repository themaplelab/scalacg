package tests.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object ExtractorWithoutArgs {

  /**
   * Testing extractors without arguments [see p. 29 of Object-Oriented Pattern Matching by Burak Emir]
   */
  @invocations("15: <unannotated> tests.matching.ExtractorWithoutArgs.UpperCase: unapply(s: String)")
  def main(args: Array[String]) {
    val s = "A"
    s match {
      case UpperCase() => {"foo"; ExtractorWithoutArgs.this}.foo()
      case _           => Console.println("wrong")
    }
  }

  @target("foo") def foo() {
    Console.println("foo")
  }

  object UpperCase {
    def unapply(s: String): Boolean =
      s.toUpperCase == s      // todo: construct test case for unapply method call?
  }
}

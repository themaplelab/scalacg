package tests.matching

import callgraph.annotation.target

object ExtractorWithoutArgs {

  /**
   * Testing extractors without arguments [see p. 29 of Object-Oriented Pattern Matching by Burak Emir]
   */
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

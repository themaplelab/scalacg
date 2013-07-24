package tests.tca.matching

import callgraph.annotation.target
import callgraph.annotation.invocations

object ExtractorVarargs {

  /**
   * Testing extractors with variable arguments [see p. 29 of Object-Oriented Pattern Matching by Burak Emir]
   */
  @invocations("17: <unannotated> scala.MatchError: <init>(obj: Any)", 
               "18: <unannotated> tests.tca.matching.ExtractorVarargs.Domain: unapplySeq(whole: String)",
               "20: <unannotated> tests.tca.matching.ExtractorVarargs.Domain: unapplySeq(whole: String)",
               "22: <unannotated> tests.tca.matching.ExtractorVarargs.Domain: unapplySeq(whole: String)")
  def main(args: Array[String]) {
    val dom = "a.b.c.d.net"
    dom match {
      case Domain("org", "acm") =>
        Console.println("wrong")
      case Domain("com", "sun", "java") =>
        Console.println("wrong")
      case Domain("net", _*) =>
        {"foo"; this}.foo()
    }
  }

  @target("foo") def foo() {
    Console.println("foo")
  }

  object Domain {
    def apply(parts: String*): String =
      parts.reverse.mkString(".")

    def unapplySeq(whole: String): Option[Seq[String]] =
      Some(whole.split("\\.").reverse)
  }

}

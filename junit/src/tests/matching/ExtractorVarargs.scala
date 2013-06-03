package tests.matching

import tests.target

object ExtractorVarargs {

  /**
   * Testing extractors with variable arguments [see p. 29 of Object-Oriented Pattern Matching by Burak Emir]
   */
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

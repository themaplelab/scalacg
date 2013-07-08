package tests

import callgraph.annotation.invocations

object Match2 {

  @invocations("11: <unannotated> tests.Match2.X: <init>(a: Int)",
               "12: <unannotated> java.lang.Object: ne(x$1: Object)", 
               "13: <unannotated> tests.Match2.X: a()")
  def main(args: Array[String]) = {
    val e = X(1)
    e match {
      case X(1) =>
        println("match")
      case _ =>
        println("no match")
    }
  }

  case class X(a: Int) {

  }

}
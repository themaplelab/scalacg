package tests.tca

import callgraph.annotation.target
import callgraph.annotation.invocations

object Match1 {
  var displayUsage = false

  def main(args: Array[String]) {
    { "handleArgs"; this}.handleArgs(args.toList)
  }

  @invocations("20: <unannotated> scala.MatchError: <init>(obj: Any)", 
              "22: <unannotated> tests.tca.Match1: displayUsage_=(x$1: Boolean)",
              "24: <unannotated> tests.tca.Match1: displayUsage_=(x$1: Boolean)")
  @target("handleArgs") 
  def handleArgs(args: List[String]) {
    val printMatcher = "--print:([a-z]+)".r

    (args.head.toLowerCase :: args.tail) match {
      case "--help" :: xs =>
        displayUsage = true
      case printMatcher(ph) :: xs =>
        displayUsage = false
      case Nil =>
    }
  }

}
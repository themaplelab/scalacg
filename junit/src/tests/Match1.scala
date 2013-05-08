package tests

object Match1 {
  var displayUsage = false

  def main(args: Array[String]) = {
    handleArgs(args.toList)
  }

  def handleArgs(args: List[String]): Unit = {
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
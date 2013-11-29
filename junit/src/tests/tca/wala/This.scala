package tests.tca.wala

object This {

  def main(args: Array[String]): Unit = {
    var s: Val = Assoc()
    s = Native()
    s.toStr
  }

  abstract class Val {
    def toStr = toString
  }

  case class Native extends Val {
    override def toString = "Native"
  }

  case class Assoc extends Val {
    override def toString = "\"" + toStr + "\""
    override def toStr = "Assoc"
  }
}
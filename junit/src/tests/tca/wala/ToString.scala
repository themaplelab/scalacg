package tests.tca.wala

object ToString {

  def main(args: Array[String]): Unit = {
    val t = TTop
    var a: Parser.ArrayEntryType = Parser.AnyEntry(t)
    a = Parser.AnyStringEntry(t)
    a = Parser.AnyIntEntry(t)
    a = Parser.Entry("foo", t)
  }

  sealed abstract class Type
  object TTop extends Type

  object Parser {
    sealed abstract class ArrayEntryType
    case class AnyEntry(t: Type) extends ArrayEntryType
    case class AnyStringEntry(t: Type) extends ArrayEntryType
    case class AnyIntEntry(t: Type) extends ArrayEntryType
    case class Entry(s: String, t: Type) extends ArrayEntryType
  }
}
package tests.tca

object Phantm1 {

  def main(args: Array[String]): Unit = {
    var t: Type = TNull
    t = TFoo
    t.toText
  }

  sealed abstract class Type {
    self =>
    def equals(t: Type) = t == self
    def toText: String = toString
  }

  sealed abstract class ConcreteType extends Type

  case object TNull extends ConcreteType {
    //    override def toText = "null"
  }

  case object TFoo extends ConcreteType {
    //    override def toText = "foo"
  }
}
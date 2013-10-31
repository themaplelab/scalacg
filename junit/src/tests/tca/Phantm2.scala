package tests.tca

object Phantm2 {

  def main(args: Array[String]): Unit = {
    var t: Type = TFloatLit(2)
    t.toText
  }

  sealed abstract class Type {
    self =>
    def equals(t: Type) = t == self
    def toText: String = toString
  }

  sealed abstract class ConcreteType extends Type
  sealed abstract class TNumericLit extends ConcreteType

  case class TFloatLit(value: Float) extends TNumericLit {
    override def toText = "Float(" + value + ")"
  }

}
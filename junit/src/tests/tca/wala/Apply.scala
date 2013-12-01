package tests.tca.wala

object Apply {

  def main(args: Array[String]): Unit = {
    val t: Type = TTop
    ApplyObject.toString
    val o = O.lookupByType(t)
    val a = new API.Writer().typeToXML(t, t => t)
  }

  object API {
    class Writer {
      def typeToXML(typ: Type, widen: Type => Type) {
        widen(typ)
      }
    }
  }

  sealed abstract class Type
  case object TTop extends Type
  class TUnion(val types: Set[Type]) extends Type

  object O {
    def lookupByType(typ: Type): Type = typ match {
      case tu: TUnion => tu.types.map(lookupByType).head
      case _ => TTop
    }
  }

  case class ApplyObject(v: String)
}
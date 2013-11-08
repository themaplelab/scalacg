package tests.tca

object Kiama1 {

  def main(args: Array[String]): Unit = {
    val p = new ParenPrettyPrinter with Foo
    val ppb = new PrettyPrinterBase with Foo
    ppb.value(LeftAssoc)
  }
  
  trait Foo
  
  trait PrettyPrinterBase {
    def value(v: Any) = v.toString
  }
  
  trait ParenPrettyPrinter {
    def toParentDoc = {
      bracket(LeftAssoc)
    }
    def bracket(s: Side) = "side"
  }
  
  trait SourcePrettyPrinter extends ParenPrettyPrinter {
    
  }
  
  abstract class Side
  case object LeftAssoc extends Side

}
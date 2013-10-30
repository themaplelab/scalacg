package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object This {

  def main(args: Array[String]) = {
    
  }
  
  type ==>[T,U] = PartialFunction[T,U]
  
  trait Super {
    @reachable
    def foo: String ==> String = {
      case _ => "foo"
    }
  }
  
  trait StringOps extends Super {
    case class StringValue(s: String)
    @reachable
    override def foo: String ==> String = super.foo orElse {
      case _ => bar
    }
    
    @reachable
    private def bar = "bar"
  }

}
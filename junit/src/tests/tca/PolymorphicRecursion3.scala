package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object PolymorphicRecursion3 {
  def main(args: Array[String]) {
    new M3(7, new Base)
  }

  class Base
  
  class S2 {
    type T
    def foo(s: T): S2 = {
      println(s.toString)
      this
    }
    @reachable override def toString = "S2"
  }
  
  class M3[U](i: Int, s: U) {
    if(i>0){
      new M3(i-1, (new S2 { type T = U }).foo(s))
    }
  }
}

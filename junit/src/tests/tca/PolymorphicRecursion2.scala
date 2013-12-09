package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object PolymorphicRecursion2 {
  def main(args: Array[String]) {
    M2(7, new Base)
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
  
  def M2[U](i: Int, s: U): Unit = {
    if(i>0){
      M2(i-1, (new S2 { type T = U }).foo(s))
    }
    
  }
}
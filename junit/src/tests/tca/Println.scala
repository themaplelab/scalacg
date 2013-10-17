package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object Println {

  val c = new C
  
  def main(args: Array[String]) = {
    println("output " + c.value)
  }
  
  class C {
    @reachable
    def value = "C"
  }

}
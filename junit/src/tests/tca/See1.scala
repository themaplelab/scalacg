package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object See1 {

  def main(args: Array[String]) = {
    val t = Foo.t
  }
  
  object Foo {
    @reachable
    def t = "t"
  }

}
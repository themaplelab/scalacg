package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object ObjectReceiver {

  def main(args: Array[String]): Unit = {
    val ret = Util.foo
    println(ret)
  }

}

object Util {
  @reachable
  def foo = "Util.foo"
}
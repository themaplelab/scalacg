package tests.ra

import scala.util.control.Breaks._
import ca.uwaterloo.scalacg.annotation.target

/**
 * Breakable is a Scala construct that allows for "break" statements. So nothing really fancy here other than the
 * fact that using "breakable" is necessary to be able to use the "break" statement. Otherwise, a run time exception
 * is thrown complaining about "scala.util.control.BreakControl".
 */
object Breakable {

  def main(args: Array[String]) {
    var sum = 0
    val a = new A()
    breakable {
      for (i <- 0 to 1000) {
        sum += { "A.one";  a }.one
        if (sum > 1000) {
          println("sum = " + sum)
          println("breaking ...")
          break
        }
      }
    }
  }

  class A {
    @target("A.one") def one: Int = { 1 }
  }
}
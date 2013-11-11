package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object Module1 {

  def main(args: Array[String]): Unit = {
    val v = A
    v.foo

    val b = p(B)
    b.toString

    C.bar
  }

  def p(any: Any) = any

  def bla = {
    object BLA
    BLA
  }

  object C {
    @reachable
    def bar = "bar"
  }
}

object A {
  @reachable
  def foo = "foo"
}

object B {
  @reachable
  override def toString = "B"
}
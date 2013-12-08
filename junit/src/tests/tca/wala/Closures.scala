package tests.tca.wala

object Closures {

  def main(args: Array[String]): Unit = {
    this.bar(foo1)
  }

  val foo1: () => A = () => { println("foo1"); new A; }
  val foo2: () => A = () => { println("foo2"); new A; }

  def bar(y: () => A) { y() }

  class A
}
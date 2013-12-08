package tests.tca.wala

object Closures {

  def main(args: Array[String]): Unit = {
	val foo1: () => A = () => { println("foo1"); new A; }
	val foo2: () => B = () => { println("foo2"); new B; }
    this.bar1(foo1)
    this.bar2(foo2)
  }

  def bar1(y: () => A) { y() }
  def bar2(z: () => B) { z() }

  class A
  class B
}
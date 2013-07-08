package tests

object LibraryCall2 {

  class A { override def hashCode() = 41 }
  class B
  trait C

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B
    val ac = new A with C
    val l = List[Int](1, 2, 3, 4)
    l.size
    l.toString
    println(a.hashCode)
    println(b.hashCode)
    println(ac.hashCode)
    println(b.toString)
  }
}
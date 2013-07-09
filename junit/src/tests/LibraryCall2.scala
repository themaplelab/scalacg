package tests

import callgraph.annotation.invocations

object LibraryCall2 {

  class A { override def hashCode = 41 }
  class B
  trait C

  @invocations("22: <unannotated> scala.collection.SeqLike: size()",
               "23: <unannotated> scala.collection.SeqLike: toString()",
               "24: <unannotated> tests.LibraryCall2.A: hashCode()",
               "25: <unannotated> java.lang.Object: hashCode()",
               "26: <unannotated> tests.LibraryCall2.A: hashCode()",
               "27: <unannotated> java.lang.Object: toString()")
  def main(args: Array[String]) {
    val a = new A
    val b = new B
    val ac = new A with C
    val l = List[Int](1, 2, 3, 4)
    l.size
    l.toString()
    println(a.hashCode)
    println(b.hashCode)
    println(ac.hashCode)
    println(b.toString)
  }
}
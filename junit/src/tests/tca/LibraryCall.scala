package tests.tca

import scala.collection.immutable.LinearSeq
import callgraph.annotation.invocations

object LibraryCall {

  trait A {
    def apply(idx: Int) = "hello";
    def length(): Int = 1;
    def size: Int;
  }

  def main(args: Array[String]): Unit = {
    val x = new A with LinearSeq[String] {};
    val s = foo(x);
    println(s);
  }

  @invocations("22: <unannotated> scala.collection.SeqLike: size()")
  def foo(a: A): Int = {
    a.size; // static target is A.size, but dispatches to LinearSeq.size
  }
}
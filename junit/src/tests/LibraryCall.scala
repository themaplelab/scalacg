package tests

import scala.collection.immutable.LinearSeq

object LibraryCall {

  trait A {
    def apply(idx : Int) = "hello";
    def length() : Int = 1;
    def size : Int;
  }
  
  def main(args: Array[String]): Unit = {
    val x = new A with LinearSeq[String] {};
    val s = foo(x);
    println(s);
    
     { "FORCE_TEST_FAILURE"; this}.fail(); // force test failure until we decide what call graph we want to compute..
  }
  
  def foo(a : A) : Int = {
   a.size;
  }
  
  def fail(){}

}
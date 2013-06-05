package tests

import callgraph.annotation.target

object PathTypes2 {
  class Outer {
    abstract class Inner {
      def foo();
    }
  }

  def main(args: Array[String]) = {
    val outer1 = new Outer 
    val outer2 = new Outer  
    
    val inner1 = new outer1.Inner {
      @target("inner1.foo") override def foo() = { 
         println("outer1.Inner.foo");
         { "bar"; this }.bar()
      }
      @target("bar") def bar() = { println("outer1.Inner.bar"); }
    }
    val inner2 = new outer2.Inner {
      @target("inner2.foo") override def foo() = println("outer2.Inner.foo")
    }
     
    { "inner1.foo"; inner1 }.foo()
//    inner2.foo()
  }
}
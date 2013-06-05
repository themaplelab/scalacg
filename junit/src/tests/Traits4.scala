package tests

import callgraph.annotation.target

object Traits4 {
  def main(args: Array[String]) = {
	  { "Y.foo"; (new Y with X1)}.foo();
	  { "Y.foo"; (new Y with X2)}.foo();
  }

  trait X1 { 
	  def foo();
  }
  trait X2 { 
	  def foo();
  }
  
  trait Y {
	  @target("Y.foo") def foo(): Unit = { }
  }
}
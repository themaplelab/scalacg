package tests.tca

import callgraph.annotation.target

object Traits7 {
  def main(args: Array[String]) = {
	  { "T1.g"; (new T1 with T2)}.g();
	  { "T1.g"; (new T1 with T2)}.g();
  }
  
  trait T1 { 
	  @target("T1.g")  def g(){} 
  }
  trait T2 { 
	  def g()
  }
}
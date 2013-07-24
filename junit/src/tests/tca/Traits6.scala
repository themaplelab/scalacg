package tests.tca

import callgraph.annotation.target

object Traits6 {
  def main(args: Array[String]) = {
	  { "T2.f"; (new T1 with T2)}.f();
	  { "T3.g"; (new T2 with T3)}.g();
  }
  trait T1 { 
	  @target("T1.g") def g(){}
  }
  trait T2 { 
      @target("T2.f") def f(){ { { "T1.g"; "T3.g" }; this}.g(); }
	  def g();
  }
  trait T3 {
	  @target("T3.g") def g(){}
  }
}
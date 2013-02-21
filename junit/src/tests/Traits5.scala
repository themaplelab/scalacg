package tests

object Traits5 {
  def main(args: Array[String]) = {
	  { "T1.f"; (new T1 with T2)}.f();
	  { "T3.h"; (new T2 with T3)}.h();
  }
  trait T1 { 
	  @target("T1.f") def f(){ { "T1.g"; this}.g(); }
	  @target("T1.g") def g(){}
  }
  trait T2 { 
	  def g();
  }
  trait T3 {
	  @target("T3.g") def g(){}
	  @target("T3.h") def h(){ { "T3.g"; this}.g(); }
  }
}
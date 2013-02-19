// CHA should compute two possible targets

class Test3 {
   
  p(new A());
  p(new B());
  
  def p(x : A) : Unit = {
    {"A.foo"; "B.foo"; x }.foo();
  }
  
}

class A {
   @target("A.foo") def foo() : Unit = { }
} 

class B extends A {
   @target("B.foo") override def foo() : Unit = { }
} 




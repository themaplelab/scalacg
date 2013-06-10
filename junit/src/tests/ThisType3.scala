package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

object ThisType3 {
    class A {
      def zip(x : this.type) : Unit = { { "A.zap"; x}.zap(); } 
       @target("A.zap") def zap() { println("A.zap"); }
    }
    class B extends A {
      @target("B.zip") override def zip(x : this.type) : Unit = { { "B.zap"; x}.zap(); } 
      @target("B.zap") override def zap()  { println("B.zap"); }
    }
    
    @invocations("19: A.zip")
    def main(args: Array[String]) = {
      val x : A = new B();
      val _ = new A();
      x.zip(x); // cannot place annotation here because the argument has type "this.type"
    }  
} 
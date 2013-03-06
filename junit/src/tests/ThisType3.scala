package tests

object ThisType3 {
    class A {
      def zip(x : this.type) : Unit = { { "A.zap"; x}.zap(); } 
       @target("A.zap") def zap() { println("A.zap"); }
    }
    class B extends A {
      override def zip(x : this.type) : Unit = { { "B.zap"; x}.zap(); } 
      @target("B.zap") override def zap()  { println("B.zap"); }
    }
    
    def main(args: Array[String]) = {
      val x : A = new B();
      x.zip(x);
    }  
} 
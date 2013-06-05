package tests

import callgraph.annotation.target

object GetterMethod1 {

  class Emitter {

    /**
     * Emit `any`.
     */
    def emit (any : Any) {
        print (any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any) {
        println (any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln {
        println
    }

}
  
  trait StdoutEmitter {
     @target("emitter") val emitter = new Emitter
  }
  
  object Foo extends StdoutEmitter {
    def bar() = {
       { "emitter"; this}.emitter.emitln ("foo")
    }
  }
  
  def main(args: Array[String]): Unit = {
   Foo.bar();
  }

}
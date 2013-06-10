package tests

import callgraph.annotation.target
import callgraph.annotation.invocations
import callgraph.annotation.notreachable

object GetterMethod1 {

  class Emitter {

    /**
     * Emit `any`.
     */
    @notreachable
    def emit (any : Any) {
        print (any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    @target("emitln") def emitln (any : Any) {
        println (any.toString)
    }

    /**
     * Emit a new line.
     */
    @notreachable
    def emitln {
        println
    }

}
  
  trait StdoutEmitter {
     val emitter = new Emitter
  }
  
  object Foo extends StdoutEmitter {
    
    @invocations("ADD assertion for call to the generated getter method for emitter")
    @target("bar") def bar() = {
       { "emitln"; this.emitter}.emitln ("foo")
    }
  }
  
  def main(args: Array[String]): Unit = {
   { "bar"; Foo}.bar();
  }

}
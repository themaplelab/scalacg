package tests.ra

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.invocations
import ca.uwaterloo.scalacg.annotation.notreachable

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
    @target("emitlnAny")
    def emitln (any : Any) {
        println (any.toString)
    }

    /**
     * Emit a new line.
     */
    @target("emitln")
    def emitln() {
        println()
    }

}
  
  trait StdoutEmitter {
     val emitter = new Emitter
  }
  
  object Foo extends StdoutEmitter {
    
    @invocations("44: <unannotated> tests.ra.GetterMethod1.StdoutEmitter: emitter()")
    @target("bar") def bar() {
       {"emitln"; "emitlnAny"; this.emitter}.emitln("foo")
    }
  }
  
  def main(args: Array[String]) {
    {"bar"; Foo}.bar()
  }

}
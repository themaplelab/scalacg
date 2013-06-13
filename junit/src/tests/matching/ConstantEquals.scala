package tests.matching

import callgraph.annotation.invocations

object ConstantEquals {

  /**
   * Testing extractors
   */
  // Karim: The call below will not be in the call graph because Nil is not instantiated in the application.
  // Our analysis only knows about the types instantiated in the code fed to the compiler (i.e., the application).
  // @invocations("14: <unannotated> scala.collection.immutable.Nil: equals()")
  def main(args: Array[String]) {
    Nil match {
      case Nil => println("right")
      case _ => println("wrong")
    }
  }
}

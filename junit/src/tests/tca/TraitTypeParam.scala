package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object TraitTypeParam {

  def main(args: Array[String]) = {
    val params = List(new Parameter[Int], new Parameter[String], new Parameter[Float])

    val result = params.head match {
      case p: HasValue[_] =>
        p.setFromString("hola")
    }
    
    println(result)
  }

  class Parameter[T] extends CommandLineOption[T] with HasValue[T]

  trait HasValue[T] extends CommandLineArgument[T] {
    val hasValue = true
    @reachable
    def setFromString(s: String) = s
  }

  trait CommandLineOption[T] extends CommandLineArgument[T]

  trait CommandLineArgument[T]

}
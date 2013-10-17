package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object MethodTypeParam {

  def main(args: Array[String]) = {
    val opt1 = option[Int](1)
    println(opt1)
    
    val opt2 = option[String]("foo")
    println(opt2)
  }
  
  @reachable
  def option[T](value: T) = {
    var opt: Option[T] = None
    opt = Some(value)
    opt
  }

}
package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object See2 {

  def main(args: Array[String]) = {
    var s: Scalar = Real(1)
    println(s.toString)
    s = Str("foo")
    println(s.toString)
    println(Real(1).toString)
  }

  trait IResult
  
  abstract class Scalar extends IResult {
    type T
    val v: T
    override def toString = v.toString // commenting this line will make Real.toString be generated in the type Real, not its companion object. 
  }

  case class Real(v: Double) extends Scalar {
    type T = Double
  }

  case class Str(v: String) extends Scalar {
    type T = String
    override def toString = "\"" + toStr + "\""
    def toStr = v
  }

}
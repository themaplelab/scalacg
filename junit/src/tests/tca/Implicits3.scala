package tests.tca

import callgraph.annotation.{target, invocations}

object Implicits3 {
  import ComplexImplicits._
   
  object ComplexImplicits {
    @target("d2c")
    implicit def Double2Complex(value : Double) = new Complex(value,0.0)
    @target("t2c")
    implicit def Tuple2Complex(value : Tuple2[Double,Double]) = new Complex(value._1,value._2)
  }
  
  class Complex(val real : Double, val imag : Double) {

  @target("+")
  def +(that: Complex) : Complex = (this.real + that.real, this.imag + that.imag)

  @target("-")
  def -(that: Complex) : Complex = (this.real - that.real, this.imag + that.imag)

  @target("~")
  def unary_~ = Math.sqrt(real * real + imag * imag)

  @target("toString")
  override def toString = real + " + " + imag + "i"
   
}
  
  @invocations("33: t2c", "34: t2c", "36: +", "37: -", "38: ~", "40: +", "42: +", "42: t2c")
  def main(args: Array[String]) {
    val a: Complex = (4.0, 5.0)
    val b: Complex = (2.0, 3.0)
    println(a)  // 4.0 + 5.0i
    println(a + b)  // 6.0 + 8.0i
    println(a - b)  // 2.0 + 8.0i
    println(~b)  // 3.60555

    var c = 4 + b
    println(c)  // 6.0 + 3.0i
    var d = (1.0,1.0) + c
    println(d)  // 7.0 + 4.0i
  }
}
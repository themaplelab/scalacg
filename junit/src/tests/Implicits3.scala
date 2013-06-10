package tests

import callgraph.annotation.invocations

object Implicits3 {
  import ComplexImplicits._
   
  object ComplexImplicits {
    implicit def Double2Complex(value : Double) = new Complex(value,0.0)
    implicit def Tuple2Complex(value : Tuple2[Double,Double]) = new Complex(value._1,value._2);
  }
  
  class Complex(val real : Double, val imag : Double) {
   
  def +(that: Complex) : Complex = (this.real + that.real, this.imag + that.imag)
   
  def -(that: Complex) : Complex = (this.real - that.real, this.imag + that.imag)
       
  def unary_~ = Math.sqrt(real * real + imag * imag)
          
  override def toString = real + " + " + imag + "i"
   
}
  
  @invocations("PLEASE_ADD_INVOCATIONS_ANNOTATIONS_FOR_ALL_CALLS_BELOW")
  def main(args: Array[String]) {
       var a : Complex = (4.0,5.0)
       var b : Complex = (2.0,3.0)
       println(a)  // 4.0 + 5.0i
       println(a + b)  // 6.0 + 8.0i
       println(a - b)  // 2.0 + 8.0i
       println(~b)  // 3.60555
       
       var c = 4 + b
       println(c)  // 6.0 + 3.0i
       var d = (1.0,1.0) + c 
       println(d)  // 7.0 + 4.0i
       
       { "FORCE_TEST_FAILURE"; this}.fail(); // to make sure that the test fails until the @invocations are checked
  }
  
  def fail(){}
}
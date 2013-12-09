package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object PolymorphicRecursion4 {
  class S[T](var s: T) {
    System.err.println(s.toString());
    @reachable override def toString = "hi"
  }
  
  class Base

  class FooBar extends S[S[S[Base]]](
      new S( new S(new Base))) {
    @reachable override def toString = "foobar"
  }

  def M[U](i: Int, s: U): Unit = {
    if (i > 0) {
      M(i - 1, new S[U](s));
    }
  }

  def main(args: Array[String]) {
    new FooBar
    new M4(7, new Base)
  }
  
  class M4[U](i: Int, s: U) {
    if(i>0){
      new M4[S[U]](i-1, new S[U](s))
    }
  }
}
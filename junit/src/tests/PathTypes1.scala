package tests

import callgraph.annotation.target

object PathTypes1 {
  class Outer {
    class Inner {
      def m() = println("Outer.Inner.m")
    }
  }

  val p1 = new Outer {
    var p1i: Inner = null
  }
  val p2 = new Outer {
    var p2i: Inner = null
  }
  val p1p1i = new p1.Inner {
    @target("p1.i.m") override def m() = println("p1.Inner.m")
  }
  val p2p2i = new p2.Inner {
    @target("p2.i.m") override def m() = println("p2.Inner.m")
  }
  p1.p1i = p1p1i
  p2.p2i = p2p2i

  // In Scala, the following assignment would be type-incorrect:
  // p1.p1i = p2p2i
  //
  // CHA should rule out the call to p1.i.m on p2.p2i, because the receiver types
  // are PathTypes1.p1.Inner and PathTypes2.p2.Inner

  def main(args: Array[String]) = {
    { "p1.i.m"; p1.p1i }.m();
    { "p2.i.m"; p2.p2i }.m();
  }

}
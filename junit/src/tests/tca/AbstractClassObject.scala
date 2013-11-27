package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable
import ca.uwaterloo.scalacg.annotation.notreachable

object AbstractClassObject {

  def main(args: Array[String]): Unit = {
    val v: Type = TString
    val s = v.toText
    println(s)
  }

  sealed abstract class Type {
    self =>

    def equals(t: Type) = t == self;

    def depth = 1;

    @notreachable
    def toText = toString
  }

  sealed abstract class ConcreteType extends Type

  case object TString extends ConcreteType {
    def foo = "foo"
    @reachable
    override def toText = "TString"
  }

  // This error is related only to case objects because we collect module types from the receiver of call sites.
  //  case class TTrue extends ConcreteType {
  //    def foo = "foo"
  //    @reachable
  //    override def toText = "TString"
  //  }

}
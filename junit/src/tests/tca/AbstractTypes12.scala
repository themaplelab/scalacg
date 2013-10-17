package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object AbstractTypes12 {
  // trait C has the actual implementation of foo
  trait C {
    type T
    @target("C.foo") def foo(t: T) = t.toString
  }
  // trait A2 gives the overall structure: a trait with a trait C2 inside it,
  // and with method foo in C2
  trait A2 {
    type U
    trait C2 {
     def foo(u: U): String
    }
    val u: U
    val c: C2
    @target("A2.go") def go = {
      println({ "C.foo"; c}.foo(u))
    }
  }
  // trait A3 instantiates U as String
  trait A3 {
    type U = String
    val u: U = "hello"
  }
  // trait A4 instantiates U as Int
  trait A4 {
    type U = Int
    val u: U = 5
  }
  // A6 connects the type T and U
  trait A6 {
    type U
    trait C6 {
      type T = U
    }
  }
  // trait A7 links the concrete implementation of foo in C
  // into the inner trait C2 inside the outer trait, and
  // links in A6 which connects T with U
  trait A7 { self: A2 with A6 =>
    val c = new C with C2 with C6
  }
  def main(args: Array[String]) = {
    { "A2.go"; (new A2 with A3 with A6 with A7)}.go;
    { "A2.go"; (new A2 with A4 with A6 with A7)}.go
  }
}
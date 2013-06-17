package tests

import callgraph.annotation.target
import callgraph.annotation.invocations

@invocations("9: foo")
object ClassInvocations {

  val v = foo()

  @target("foo")
  def foo() {
    println("hello")
  }

  def main(args: Array[String]) {}
}
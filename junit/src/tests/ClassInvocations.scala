package tests

import callgraph.annotation.target
import callgraph.annotation.invocations
import callgraph.annotation.noInvocations

@invocations("10: foo")
object ClassInvocations {

  val v = foo()

  @target("foo")
  def foo() {
    println("hello")
  }
  
  @noInvocations
  def bar() {
  }

  def main(args: Array[String]) {
    bar
  }
}
package tests.tca

import callgraph.annotation.target

object Experiment {
  
  
  def main(args: Array[String]) : Unit = {
    A.foo()
    A.foo()
    A.foo()
  }

  object A {
    def foo() = {}
  } 
}
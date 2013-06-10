package tests

import callgraph.annotation.reachable

object Generics16 {

  def main(args: Array[String]) = {
    val v = new SCC[Int]
    println(v.foo)
    
  }
  
  class SCC[S] {
    @reachable
    def foo = "foo"
  }
}
package tests.tca

object Foreach1 {

  def main(args: Array[String]): Unit = {
    
    // This calls A.apply
    for(v <-  0 to 10) {
      A(v)
    }
    
    // This calls B.apply
    for(v <-  0 to 10) {
      B(v)
    }

  }

  object A {
    def apply(x: Int) = { "A" + x }
  }

  object B {
    def apply(x: Int) = { "B" + x }
  }

}
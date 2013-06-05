package tests

object MultipleAnnotations {

  @invocations("A.apply")
  @invocations("B.apply")
  def main(args: Array[String]) = {
    // This calls A.apply
    for (v <- 0 to 10) {
      A(v)
    }

    // This calls B.apply
    for (v <- 0 to 10) {
      B(v)
    }
  }

  object A {
    @target("A.apply")
    def apply(x: Int) = { "A" + x }
  }

  object B {
    @target("B.apply")
    def apply(x: Int) = { "B" + x }
  }

}
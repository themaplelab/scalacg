package tests

object AbstractTypes13 {
  trait HasFoo {
    def foo: String
  }
  
  class R {
    class A extends HasFoo {
      def foo = "R.A.foo"
    }
  }

  trait X {
    class A extends HasFoo {
      def foo = "X.A.foo"
    }
    val o = new A
  }

  trait Y {
    type A
    type B = A
  }

  trait Z {
    type B <: HasFoo
    val o: B
    def go = println(o.foo)
  }

  def main(args: Array[String]) {
    (new X with Y with Z {}).go
  }

}
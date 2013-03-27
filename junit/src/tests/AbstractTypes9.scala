package tests

object AbstractTypes9 {
  trait Shape {
    def draw(): String
  }
  trait FourSides
  trait Circle extends Shape {
    @target("Circle.draw") def draw() = "circle"
  }
  trait Square extends Shape with FourSides {
    @target("Square.draw") def draw() = "square"
  }
  trait T {
    type U <: Shape
    type V <: U with FourSides
    type W = V
    def bar(u: U, v: V, w: W) {
      { "Square.draw"; "Circle.draw"; u }.draw()

      { "Square.draw"; v }.draw()

      { "Square.draw"; w }.draw()
    }
  }
  def main(args: Array[String]): Unit = {
    val x = new T {
      type U = Shape
      type V = Shape with FourSides
    }
  }
}
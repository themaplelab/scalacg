package tests.tca

import ca.uwaterloo.scalacg.annotation.target
import ca.uwaterloo.scalacg.annotation.notreachable

object AbstractTypes9b {
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
    @notreachable def bar(u: U, v: V, w: W) {
      { "__NONE__"; u }.draw()

      { "__NONE__"; v }.draw()

      { "__NONE__"; w }.draw()
    }
  }
  def main(args: Array[String]): Unit = {
    val x = new T {
      type U = Shape
      type V = Shape with FourSides
    }
    new Circle {}
    new Square {}
  }
}
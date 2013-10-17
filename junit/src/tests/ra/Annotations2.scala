package tests.ra

import ca.uwaterloo.scalacg.annotation.{target, invocations}

object Annotations2 {

  @target("hello")
  def hello() {}

  @invocations("12: hello")
  def main(args: Array[String]) {
    {"hello"; this}.hello()
  }
}
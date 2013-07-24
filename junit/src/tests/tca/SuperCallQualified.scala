package tests.tca

import callgraph.annotation.{ target, invocations }

class Trallala {
  def bam() = "Trallala.bam"
}

trait Lalala {
  @target("Lalala.bam")
  def bam() = "Lalala.bam"
}

object SuperCallQualified extends Trallala with Lalala {

  override def bam() = "SuperCallQualified.bam"

  class B {
    @invocations("21: Lalala.bam")
    def m() {
      SuperCallQualified.super[Lalala].bam()
    }
  }

  def main(args: Array[String]) {
    new B().m() // == "Lalala.bam"
  }
}
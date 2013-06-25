package tests.ra

import callgraph.annotation.{notreachable, noInvocations}

object Annotations1 {

  @notreachable
  def notReachable() {}

  @noInvocations
  def main(args: Array[String]) {
  }
}
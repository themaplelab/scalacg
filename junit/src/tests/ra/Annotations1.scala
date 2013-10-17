package tests.ra

import ca.uwaterloo.scalacg.annotation.{notreachable, noInvocations}

object Annotations1 {

  @notreachable
  def notReachable() {}

  @noInvocations
  def main(args: Array[String]) {
  }
}
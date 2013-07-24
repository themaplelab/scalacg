package tests.tca

import callgraph.annotation.reachable

object DefInDef {

  def main(args: Array[String]) = {
    Types.typeChecking
  }

  object Types {
    @reachable
    def typeChecking = {
      @reachable
      def checkAccess = {
        println("typeChecking.checkAccess")
      }
      checkAccess
    }
  }
}
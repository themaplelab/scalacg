package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object ClassTypeParam2 {

  def main(args: Array[String]) = {
    val parser = new Parser[Int]
    val result = parseAll(parser)
    
    println(result)
  }
  
  @reachable
  def parseAll(p: Parser[_]): ParseResult[_] = {
    p.parse
  }

  class Parser[T] {
    @reachable
    def parse[T] = {
      new ParseResult[T]
    }
  }

  class ParseResult[T] {
    
  }
}
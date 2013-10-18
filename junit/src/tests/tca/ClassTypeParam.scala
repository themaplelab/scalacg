package tests.tca

import ca.uwaterloo.scalacg.annotation.reachable

object ClassTypeParam {

  def main(args: Array[String]) = {
    val parser = new Parser[Int]
    val result = parseAll(parser)
    
    println(result)
  }
  
  @reachable
  def parseAll[T](p: Parser[T]): ParseResult[T] = {
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
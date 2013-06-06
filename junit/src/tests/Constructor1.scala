package tests

import scala.util.control.Breaks._

/**
 * The simplest version of Breakable2  
 *
 * June, 6th, 2013.
 *
 * @author Karim Ali
 */
object Constructor1 {

  def main(args: Array[String]) = {
    val dc = new DumpCollector("path")
  }

  class DumpCollector(i: Int) {
    var lineNr = 3
    var path: String = ""
    var files: List[String] = Nil
    lineNr += files.size + 1 // Note this line
    
    def this(p: String) = {
      this(1)
      path = p
    }
  }
}
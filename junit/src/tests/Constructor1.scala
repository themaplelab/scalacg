package tests

import scala.util.control.Breaks._
import callgraph.annotation.invocations

/**
 * The simplest version of Breakable2  
 *
 * June, 6th, 2013.
 *
 * @author Karim Ali
 */
object Constructor1 {

  @invocations("17: <unannotated> tests.Constructor1.DumpCollector: <init>(p: String)")
  def main(args: Array[String]) = {
    val dc = new DumpCollector("path")
    
    // need @invocations annotation for the initialization code
    { "FORCE_TEST_FAILURE"; this}.fail();
  }
  
  def fail(){}

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
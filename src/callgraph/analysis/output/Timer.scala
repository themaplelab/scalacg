package callgraph.analysis.output

import scala.compat.Platform

object Timer {
  
  private var _start : Long = 0L
  
  def elapsed = (Platform.currentTime - _start) / 1000.0
  def start = _start = Platform.currentTime

}
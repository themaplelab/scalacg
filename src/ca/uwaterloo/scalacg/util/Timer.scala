package ca.uwaterloo.scalacg.util

object Timer {
  
  var start : Long = 0L
  var end : Long = 0L
  
  def elapsed = (end - start) / 1000.0

}
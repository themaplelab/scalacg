package ca.uwaterloo.scalacg.util

import scala.compat.Platform

/**
 * Time an action.
 */
object Timer {
  var s = 0L
  
  def start = s = Platform.currentTime

  def elapsed = round((Platform.currentTime - s) / 1000.0)

  def round(value: Double, places: Int = 2) = {
    assert(places >= 0, "Can't round to -ve decimal places.")

    val factor = math.pow(10, places);
    val tmp = math.round(value * factor);
    tmp / factor;
  }
}
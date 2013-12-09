package ca.uwaterloo.scalacg.util

import scala.compat.Platform

/**
 * Some math utilities.
 */
object Math {
  def round(value: Double, places: Int = 2) = {
    assert(places >= 0, "Can't round to -ve decimal places.")

    val factor = math.pow(10, places);
    val tmp = math.round(value * factor);
    tmp / factor;
  }

  def percentage(value: Double, denom: Double) = {
    round(value * 100 / denom).toInt
  }
}
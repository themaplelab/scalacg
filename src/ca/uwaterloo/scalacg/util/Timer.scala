package ca.uwaterloo.scalacg.util

import scala.compat.Platform

/**
 * Time an action.
 */
object Timer {
  var s = 0L

  def start = s = Platform.currentTime

  def elapsed = Math.round((Platform.currentTime - s) / 1000.0)
}
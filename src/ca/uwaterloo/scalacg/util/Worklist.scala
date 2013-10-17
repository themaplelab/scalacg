package ca.uwaterloo.scalacg.util

import scala.collection.mutable.Set

class Worklist[A] {
  lazy val reachableItems = Set[A]()
  lazy val newItems = Set[A]()

  def +=(elem: A) = {
    // No new elements are accepted if they've already been reachable before
    if (!reachableItems(elem)) {
      newItems += elem
      reachableItems += elem
    }
  }

  /**
   * Add new items to the worklist. It also adds the items to the reachable set.
   */
  def ++=[S <: scala.collection.Set[A]](xs: S): this.type = { xs.seq foreach += ; this }

  /**
   * Clear the new items
   */
  def clear = {
    newItems.clear
  }

  /**
   * Do we have new items to process?
   */
  def nonEmpty = {
    newItems.nonEmpty
  }
  
  /**
   * How many new items do we have?
   */
  def size = {
    newItems.size
  }
}
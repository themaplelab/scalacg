package ca.uwaterloo.scalacg.experiments

import scala.collection.mutable.Map

object SortedOutput {
  def main(args: Array[String]): Unit = {
    val path = "dumps/nov28/tca-wala/kiama/edges"
    val methodToEdges = Map[String, Set[String]]().withDefaultValue(Set.empty[String])

    for (line <- io.Source.fromFile(path).getLines) {
      val toks = line.split(" ===> ")
      if (toks.length == 2) methodToEdges(toks(0)) += toks(1)
    }

    val sorted = methodToEdges.keys.toList.sortWith((a, b) => methodToEdges(a).size > methodToEdges(b).size)
    sorted.foreach(println)
  }
}
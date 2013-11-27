package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.collection.mutable.Map

import probe.GXLReader
import probe.ProbeMethod

object SortedOutput {

  // Implicitly converts a Java parameterless Set to a parameterized Scala set.
  implicit def asScalaSet[A](set: java.util.Set[_]): Set[A] = {
    var ret = Set.empty[A]
    val iter = set.iterator
    while (iter.hasNext) {
      ret += iter.next.asInstanceOf[A]
    }
    ret
  }

  def main(args: Array[String]): Unit = {
    val path = "dumps/nov25/tca-wala/phantm/edges"
    val methodToEdges = Map[String, Set[String]]().withDefaultValue(Set.empty[String])

    for (line <- io.Source.fromFile(path).getLines) {
      val toks = line.split(" ===> ")
      if (toks.length == 2) methodToEdges(toks(0)) += toks(1)
    }

    val sorted = methodToEdges.keys.toList.sortWith((a, b) => methodToEdges(a).size > methodToEdges(b).size)
    sorted.foreach(println)

  }

}

package ca.uwaterloo.scalacg.experiments

import scala.collection.mutable.Map
import probe.ObjectManager

object Counter {
  def main(args: Array[String]): Unit = {
    val path = "dumps/nov28/tca-wala/phantm/edges"
    val methodToCount = Map[String, Int]().withDefaultValue(0)
    val nameToCount = Map[String, Int]().withDefaultValue(0)

    for (line <- io.Source.fromFile(path).getLines) {
      val toks = line.split(" ===> ")
      if (toks.length == 2) {
        methodToCount(toks(1)) += 1
        val name = toks(1).substring(toks(1).indexOf(':') + 2, toks(1).indexOf('('));
        nameToCount(name) += 1
      }
    }

    val sorted = methodToCount.keys.toList.sortWith((a, b) => methodToCount(a) > methodToCount(b))
    val sortedName = nameToCount.keys.toList.sortWith((a, b) => nameToCount(a) > nameToCount(b))
    sortedName.foreach { m => println(m + " :: " + nameToCount(m)) }
    println("=" * 50)
    sorted.foreach { m => println(m + " :: " + methodToCount(m)) }
  }
}
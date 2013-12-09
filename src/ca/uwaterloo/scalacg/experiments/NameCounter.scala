package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.collection.JavaConversions.asScalaSet
import scala.collection.mutable.Map

import ca.uwaterloo.scalacg.util.Math
import probe.TextReader

object NameCounter {
  def main(args: Array[String]): Unit = {
    val benchmarks = List("argot", "ensime", "fimpp", "joos", "kiama", "phantm", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")
    val base = "dist"

    for (benchmark <- benchmarks) {
      val nameToCount = Map[String, Int]().withDefaultValue(0)

      val names = new TextReader().readCallGraph(new GZIPInputStream(new FileInputStream(s"$base/ra-inst/$benchmark/callgraph-summary.gxl.gzip")))
      val bounds = new TextReader().readCallGraph(new GZIPInputStream(new FileInputStream(s"$base/tca-bounds/$benchmark/callgraph-summary.gxl.gzip")))
      val diff = names.edgesIgnoringContext -- bounds.edgesIgnoringContext
      for (edge <- diff) {
        nameToCount(edge.dst.name) += 1
      }

      val methods = nameToCount.keys.toList.sortWith((a, b) => nameToCount(a) > nameToCount(b))
      println(s"$benchmark :: ${methods map (m => s"($m, ${Math.percentage(nameToCount(m), diff.size)}%)")}")
    }
  }
}
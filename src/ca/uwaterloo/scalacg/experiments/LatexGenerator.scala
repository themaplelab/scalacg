package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.io.PrintStream
import java.util.zip.GZIPInputStream

object LatexGenerator {

  //  final val benchmarks = List("argot", "ensime", "fimpp", "joos", "kiama", "phantm", "scalariform", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")
  final val benchmarks = List("tictactoe")

  //  val tca_this = ("tca-this-super", Map[String, CallGraph]().withDefaultValue(new CallGraph()))
  //  val tca_std = ("tca-super", Map[String, CallGraph]().withDefaultValue(new CallGraph()))
  //  val ba = ("ba-super", Map[String, CallGraph]().withDefaultValue(new CallGraph()))
  //  val tcra = ("tcra", Map[String, CallGraph]().withDefaultValue(new CallGraph()))
  //  val ra = ("ra", Map[String, CallGraph]().withDefaultValue(new CallGraph()))
  val base = "../scalabench/local/dist"

  def main(args: Array[String]): Unit = {
    val out = new PrintStream("paper_data.tex")

    val nodes = "nodes"
    val edges = "edges"

    val tca_this_analysis = "tca this"
    val tca_std_analysis = "tca std"
    val ba_analysis = "ba"
    val tcra_analysis = "tcra"
    val ra_analysis = "ra"

    for (benchmark <- benchmarks) {
      def emit(analysis: String, key: String, value: Int) = {
        out.println(s"\\pgfkeyssetvalue{$analysis $benchmark $key}{$value}")
      }

      // Read the call graphs for this benchmark
      println(base + "/tca-this-super/" + benchmark + "/callgraph.gxl.gzip")
      val tca_this = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tca-this-super/" + benchmark + "/callgraph.gxl.gzip")))
      println(tca_this.findReachables.size)
      println(tca_this.edges.size)
      emit(tca_this_analysis, nodes, tca_this.findReachables.size)
      emit(tca_this_analysis, edges, tca_this.edges.size)

      val tca_std = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tca-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tca_std_analysis, nodes, tca_std.findReachables.size)
      emit(tca_std_analysis, edges, tca_std.edges.size)

      val ba = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/ba-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(ba_analysis, nodes, ba.findReachables.size)
      emit(ba_analysis, edges, ba.edges.size)

      val tcra = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tcra-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tcra_analysis, nodes, tcra.findReachables.size)
      emit(tcra_analysis, edges, tcra.edges.size)

      val ra = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/ra/" + benchmark + "/callgraph.gxl.gzip")))
      emit(ra_analysis, nodes, ra.findReachables.size)
      emit(ra_analysis, edges, ra.edges.size)
    }
  }
}
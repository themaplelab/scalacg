package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.io.PrintStream
import java.util.zip.GZIPInputStream

object LatexGenerator {

  final val benchmarks = List("argot", "ensime", "fimpp", "joos", "kiama", "phantm", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")
  final val analyses = List("RA", "TCRA", "BA", "TCA$_{std}$", "TCA$_{this}$", "WALA$_{rta}$")
  final val statistics = List("LOC", "# classes", "# modules", "# traits", "# mixins", "#methods", "# closures")
  final val algorithms = analyses.dropRight(1).mkString(", ") + ", and " + analyses.last

  val base = "dist"

  def main(args: Array[String]): Unit = {
    val out = new PrintStream("paper_data.tex")
    val table_results = new PrintStream("table_results.tex")
    val table_benchmarks = new PrintStream("table_benchmarks.tex")

    val nodes = "nodes"
    val edges = "edges"
    val classes = "classes"
    val modules = "modules"
    val traits = "traits"
    val mixins = "mixins"
    val closures = "closures" // these include anonfun

    val tca_this_analysis = "tca this"
    val tca_std_analysis = "tca std"
    val ba_analysis = "ba"
    val tcra_analysis = "tcra"
    val ra_analysis = "ra"
    val wala_analysis = "wala"
    val bench = "benchmark"

    def emitTableHeaders = {
      // The results table
      table_results.println("\\begin{table}")
      table_results.println("\\centering")
      table_results.println("\\resizebox{\\columnwidth}{!} {")
      table_results.println("  \\begin{tabular}{l" + ("|r" * (analyses.size - 1) * 2) + "||r|r" + "}")
      table_results.println("    \\toprule")
      table_results.println("    " + (analyses.map(a => s"& \\multicolumn{2}{|c}{\\textbf{$a}} ").mkString) + "\\\\")
      table_results.println("    " + ("& \\textbf{nodes} & \\textbf{edges} " * analyses.size) + "\\\\")
      
      // The benchmarks table
      table_results.println("\\begin{table}")
      table_results.println("\\centering")
      table_results.println("\\resizebox{\\columnwidth}{!} {")
      table_results.println("  \\begin{tabular}{l" + ("|r" * analyses.size) + "}")
      table_results.println("    \\toprule")
      table_results.println("    " + (analyses.map(a => s"& \\multicolumn{2}{|c}{\\textbf{$a}} ").mkString) + "\\\\")
      table_results.println("    " + ("& \\textbf{nodes} & \\textbf{edges} " * analyses.size) + "\\\\")
    }

    def emitTableFooters = {
      table_results.println("    \\bottomrule")
      table_results.println("  \\end{tabular}")
      table_results.println("}")
      table_results.println("  \\caption{Number of nodes and edges in call graphs computed using the " + algorithms + " algorithms.}")
      table_results.println("  \\label{table:Results}")
      table_results.println("\\end{table}")
    }

    emitTableHeaders

    for (benchmark <- benchmarks) {
      var row = new StringBuilder("    ")

      def emit(analysis: String, nodesOrEdges: String, v: Int) = {
        val key = analysis + " " + benchmark + " " + nodesOrEdges
        val value = "%,d" format v
        out.println(s"\\pgfkeyssetvalue{$key}{$value}")
        row append s" & \\pgfkeysvalueof{$key}" // add the key to the current row
      }

      // add benchmark name in italics
      row append s"\\textit{$benchmark}"

      // Read the call graphs for this benchmark
      val ra = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/ra/" + benchmark + "/callgraph.gxl.gzip")))
      emit(ra_analysis, nodes, ra.findReachables.size)
      emit(ra_analysis, edges, ra.edges.size)

      val tcra = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tcra-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tcra_analysis, nodes, tcra.findReachables.size)
      emit(tcra_analysis, edges, tcra.edges.size)

      val ba = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/ba-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(ba_analysis, nodes, ba.findReachables.size)
      emit(ba_analysis, edges, ba.edges.size)

      val tca_std = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tca-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tca_std_analysis, nodes, tca_std.findReachables.size)
      emit(tca_std_analysis, edges, tca_std.edges.size)

      val tca_this = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tca-this-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tca_this_analysis, nodes, tca_this.findReachables.size)
      emit(tca_this_analysis, edges, tca_this.edges.size)

      val wala = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/wala/" + benchmark + "/wala-callgraph.gxl.gzip")))
      emit(wala_analysis, nodes, wala.findReachables.size)
      emit(wala_analysis, edges, wala.edges.size)

      // end the row
      table_results.println("    \\midrule")
      table_results.println(row append " \\\\")
    }

    emitTableFooters

    out.close
    table_results.close
  }
}
package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.io.PrintStream
import java.util.zip.GZIPInputStream
import java.text.DecimalFormat

object LatexGenerator {

  final val benchmarks = List("argot", "ensime", "fimpp", "joos", "kiama", "phantm", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")
  final val analyses = List("RA$_{all}$", "RA$_{inst}$", "TCA$_{bounds}$", "TCA$_{expand}$", "TCA$_{expand_{this}}$", "WALA$_{rta}$")
  final val algorithms = analyses.dropRight(1).mkString(", ") + ", and " + analyses.last 
  final val chatacteristics = List("LOC") ++ List("classes", "modules", "traits", "mixins", "methods", "closures").map(a => s"\\texttt{\\#}$a")
  final lazy val floatFormat = new DecimalFormat("#,###.##")
  final lazy val intFormat = "%,d"

  val base = "dist"

  def main(args: Array[String]): Unit = {
    val out = new PrintStream("paper_data.tex")
    val table_results = new PrintStream("table_results.tex")
    val table_benchmarks = new PrintStream("table_benchmarks.tex")
    val table_time = new PrintStream("table_time.tex")

    // keys for table of charactersitcs
    val bench = "benchmark"
    val loc = "LOC"
    val classes = "classes"
    val modules = "modules"
    val traits = "traits"
    val mixins = "mixins"
    val methods = "methods"
    val closures = "closures" // these include anonfun

    // keys for table of analyses
    val tca_expand_this = "tca expand this"
    val tca_expand = "tca expand"
    val tca_bounds = "tca bounds"
    val ra_inst = "ra inst"
    val ra_all = "ra all"
    val wala_rta = "wala rta"
    val nodes = "nodes"
    val edges = "edges"

    // keys for table of times
    val time = "time"
    val scalac = "scalac"

    def emitTableHeaders = {
      // The results table
      table_results.println("\\begin{table}")
      table_results.println("\\centering")
      table_results.println("\\resizebox{\\columnwidth}{!} {")
      table_results.println("  \\begin{tabular}{l" + ("r" * analyses.size * 2) + "}")
      table_results.println("    \\toprule")
      table_results.println("    " + (analyses.map(a => s"& \\multicolumn{2}{c}{\\textbf{$a}} ").mkString) + "\\\\")
      table_results.println("    " + (analyses.indices.map(i => s"\\cmidrule(lr){${2 + 2 * i}-${3 + 2 * i}} ").mkString))
      table_results.println("    " + ("& \\textbf{nodes} & \\textbf{edges} " * analyses.size) + "\\\\")

      // The benchmarks table
      table_benchmarks.println("\\begin{table}")
      table_benchmarks.println("\\centering")
      table_benchmarks.println("\\resizebox{\\columnwidth}{!} {")
      table_benchmarks.println("  \\begin{tabular}{l" + ("r" * chatacteristics.size) + "}")
      table_benchmarks.println("    \\toprule")
      table_benchmarks.println("    " + (chatacteristics.map(a => s"& \\textbf{$a} ").mkString) + "\\\\")

      // The times table
      table_time.println("\\begin{table}")
      table_time.println("\\centering")
      table_time.println("\\resizebox{\\columnwidth}{!} {")
      table_time.println("  \\begin{tabular}{l" + ("r" * analyses.size) + "r" + "}")
      table_time.println("    \\toprule")
      table_time.println("    " + (analyses.map(a => s"& \\textbf{$a} ").mkString) + s"& \\textbf{$scalac} " + "\\\\")
    }

    def emitTableFooters = {
      // The results table
      table_results.println("    \\bottomrule")
      table_results.println("  \\end{tabular}")
      table_results.println("}")
      table_results.println("  \\caption{Number of nodes and edges in call graphs computed using the " + algorithms + " algorithms.}")
      table_results.println("  \\label{table:Results}")
      table_results.println("\\end{table}")

      // The benchmarks table
      table_benchmarks.println("    \\bottomrule")
      table_benchmarks.println("  \\end{tabular}")
      table_benchmarks.println("}")
      table_benchmarks.println("  \\caption{Benchmark characteristics.}")
      table_benchmarks.println("  \\label{table:Benchmarks}")
      table_benchmarks.println("\\end{table}")

      // The time table
      table_time.println("    \\bottomrule")
      table_time.println("  \\end{tabular}")
      table_time.println("}")
      table_time.println("  \\caption{The time taken by the " + algorithms + " algorithms to compute the call graphs.}")
      table_time.println("  \\label{table:Time}")
      table_time.println("\\end{table}")
    }

    emitTableHeaders

    for (benchmark <- benchmarks) {
      var resultsRow = new StringBuilder("    ")
      var benchRow = new StringBuilder("    ")
      var timesRow = new StringBuilder("    ")
      val logfile = io.Source.fromFile(base + "/stats/" + benchmark + "/stats-log").getLines.toList

      def emit(analysis: String, k: String, v: Int) = {
        val key = analysis + " " + benchmark + " " + k
        val value = intFormat format v
        out.println(s"\\pgfkeyssetvalue{$key}{$value}")
        resultsRow append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row
      }

      def emitBench(k: String, v: Int) = {
        val key = bench + " " + benchmark + " " + k
        val value = intFormat format v
        out.println(s"\\pgfkeyssetvalue{$key}{$value}")
        benchRow append s" & \\pgfkeysvalueof{$key}" // add the key to the current benchmarks row
      }

      def emitTime(analysis: String, v: Float) = {
        val key = analysis + " " + benchmark + " " + time
        val value = floatFormat format v
        out.println(s"\\pgfkeyssetvalue{$key}{$value}")
        timesRow append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row
      }

      def extract(what: String) = logfile.find(_ contains what).get.split(":").last.trim.toInt
      def nClasses = extract("# classes")
      def nModules = extract("# modules")
      def nTraits = extract("# traits")
      def nMixins = extract("# mixins")
      def nMethods = extract("# methods")
      def nClosures = extract("# anonfun") + extract("# closures")
      def nLoc = extract("# loc")

      def extractAnalysisTime(log: List[String]) = log.find(_ contains "Finished callgraph in").get.split(" ").dropRight(1).last.trim.toFloat
      def time_ra_all = extractAnalysisTime(io.Source.fromFile(base + "/ra/" + benchmark + "/ra-log").getLines.toList)
      def time_ra_inst = extractAnalysisTime(io.Source.fromFile(base + "/tcra-super/" + benchmark + "/tcra-log").getLines.toList)
      def time_tca_bounds = extractAnalysisTime(io.Source.fromFile(base + "/ba-super/" + benchmark + "/ba-log").getLines.toList)
      def time_tca_expand = extractAnalysisTime(io.Source.fromFile(base + "/tca-super/" + benchmark + "/tca-std-log").getLines.toList)
      def time_tca_expand_this = extractAnalysisTime(io.Source.fromFile(base + "/tca-this-super/" + benchmark + "/tca-this-log").getLines.toList)
      def time_wala_rta = io.Source.fromFile(base + "/wala/" + benchmark + "/wala-log").getLines.toList.find(_ contains "WALA took:").get.split(":").last.trim.toFloat
      def time_scalac = io.Source.fromFile(base + "/scalac/" + benchmark + "/scalac-log").getLines.toList.find(_ contains "Total time:").get.split(" ").dropRight(1).last.trim.toInt

      // add benchmark name in italics
      resultsRow append s"\\textit{$benchmark}"
      benchRow append s"\\textit{$benchmark}"
      timesRow append s"\\textit{$benchmark}"

      // Read the call graphs for this benchmark
      val ra = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/ra/" + benchmark + "/callgraph.gxl.gzip")))
      emit(ra_all, nodes, ra.findReachables.size)
      emit(ra_all, edges, ra.edges.size)

      val tcra = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tcra-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(ra_inst, nodes, tcra.findReachables.size)
      emit(ra_inst, edges, tcra.edges.size)

      val ba = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/ba-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tca_bounds, nodes, ba.findReachables.size)
      emit(tca_bounds, edges, ba.edges.size)

      val tca_std = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tca-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tca_expand, nodes, tca_std.findReachables.size)
      emit(tca_expand, edges, tca_std.edges.size)

      val tca_this = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/tca-this-super/" + benchmark + "/callgraph.gxl.gzip")))
      emit(tca_expand_this, nodes, tca_this.findReachables.size)
      emit(tca_expand_this, edges, tca_this.edges.size)

      val wala = new probe.TextReader().
        readCallGraph(new GZIPInputStream(new FileInputStream(base + "/wala/" + benchmark + "/wala-callgraph.gxl.gzip")))
      emit(wala_rta, nodes, wala.findReachables.size)
      emit(wala_rta, edges, wala.edges.size)
      table_results.println("    \\midrule")
      table_results.println(resultsRow append " \\\\")

      // Read the stats log file
      emitBench(loc, nLoc)
      emitBench(classes, nClasses)
      emitBench(modules, nModules)
      emitBench(traits, nTraits)
      emitBench(mixins, nMixins)
      emitBench(methods, nMethods)
      emitBench(closures, nClosures)
      table_benchmarks.println("    \\midrule")
      table_benchmarks.println(benchRow append " \\\\")

      // Read the time info
      emitTime(ra_all, time_ra_all)
      emitTime(ra_inst, time_ra_inst)
      emitTime(tca_bounds, time_tca_bounds)
      emitTime(tca_expand, time_tca_expand)
      emitTime(tca_expand_this, time_tca_expand_this)
      emitTime(wala_rta, time_wala_rta)
      emitTime(scalac, time_scalac)
      table_time.println("    \\midrule")
      table_time.println(timesRow append " \\\\")
    }

    emitTableFooters

    out.close
    table_results.close
    table_benchmarks.close
    table_time.close
  }
}
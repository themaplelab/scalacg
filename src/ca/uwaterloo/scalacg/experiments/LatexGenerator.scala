package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.io.PrintStream
import java.text.DecimalFormat
import java.util.zip.GZIPInputStream
import probe.TextReader
import ca.uwaterloo.scalacg.util.Timer

object LatexGenerator {

  final val benchmarks = List("argot", "ensime", "fimpp", "joos", "kiama", "phantm", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")

  final val analyses = List("\\raAll", "\\raInst", "\\tcaBounds", "\\tcaExpand", "\\tcaExpandThis", "\\walaRta")
  final val algorithms = analyses.dropRight(1).mkString(", ") + ", and " + analyses.last

  final val chatacteristics = List("LOC") ++ List("classes", "objects", "traits", "trait comp.", "methods", "closures").map(a => s"\\texttt{\\#} $a")

  final lazy val floatFormat = new DecimalFormat("#,###.##")
  final lazy val intFormat = "%,d"

  // constant file names
  final val cg = "callgraph-summary.gxl.gzip"
  final val walacg = "wala-" + cg
  final val log = "tca-expand-this-log"

  // keys for table of characteristics
  final val bench = "benchmark"
  final val loc = "LOC"
  final val classes = "classes"
  final val modules = "modules"
  final val traits = "traits"
  final val mixins = "mixins"
  final val methods = "methods"
  final val closures = "closures" // these include anonfun

  // keys for table of analyses
  final val tca_expand_this = "tca expand this"
  final val tca_expand = "tca expand"
  final val tca_bounds = "tca bounds"
  final val ra_inst = "ra inst"
  final val ra_all = "ra all"
  final val wala_rta = "wala rta"
  final val nodes = "nodes"
  final val edges = "edges"

  // keys for table of times
  final val time = "time"
  final val scalac = "scalac"

  final val base = "dist"

  def main(args: Array[String]): Unit = {
    val data = new PrintStream("paper_data.tex")

    // Emit latex files
    emitTableResults
    emitTableBenchmarks
    emitTableTimes

    data.close

    def readCallGraph(file: String) = new TextReader().readCallGraph(new GZIPInputStream(new FileInputStream(file)))

    // Emit the results table with all the nodes, edges in it.
    def emitTableResults = {
      val table = new PrintStream("table_results.tex")

      // Emit Header
      table.println("\\begin{table}")
      table.println("\\centering")
      table.println("  \\begin{tabularx}{\\columnwidth}{ll" + ("R" * analyses.size) + "}")
      table.println("    \\toprule")
      table.println("    & " + (analyses.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + "\\\\")

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")

        // add benchmark name in italics
        row append s"\\multirow{2}{*}{\\textit{$benchmark}}"

        // Read the call graphs for this benchmark
        lazy val ra = readCallGraph(base + "/ra-all/" + benchmark + "/" + cg)
        lazy val tcra = readCallGraph(base + "/ra-inst/" + benchmark + "/" + cg)
        lazy val ba = readCallGraph(base + "/tca-bounds/" + benchmark + "/" + cg)
        lazy val tca_std = readCallGraph(base + "/tca-expand/" + benchmark + "/" + cg)
        lazy val tca_this = readCallGraph(base + "/tca-expand-this/" + benchmark + "/" + cg)
        lazy val wala = readCallGraph(base + "/wala-rta/" + benchmark + "/" + walacg)

        // Emit nodes
        row append s" & $nodes"
        emit(ra_all, nodes, ra.findReachables.size)
        emit(ra_inst, nodes, tcra.findReachables.size)
        emit(tca_bounds, nodes, ba.findReachables.size)
        emit(tca_expand, nodes, tca_std.findReachables.size)
        emit(tca_expand_this, nodes, tca_this.findReachables.size)
        emit(wala_rta, nodes, wala.findReachables.size)
        table.println("    \\midrule")
        table.println(row append " \\\\")
        table.println("    \\cmidrule{2-8}")
        row.clear

        // Emit edges
        row append s" & $edges"
        emit(ra_all, edges, ra.edgesIgnoringContext.size)
        emit(ra_inst, edges, tcra.edgesIgnoringContext.size)
        emit(tca_bounds, edges, ba.edgesIgnoringContext.size)
        emit(tca_expand, edges, tca_std.edgesIgnoringContext.size)
        emit(tca_expand_this, edges, tca_this.edgesIgnoringContext.size)
        emit(wala_rta, edges, wala.edgesIgnoringContext.size)
        table.println(row append " \\\\")
        //        if (benchmark != benchmarks.last) table.println("    \\addlinespace")

        def emit(analysis: String, k: String, v: Int) = {
          val key = analysis + " " + benchmark + " " + k
          val value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row
        }
      }

      // Emit Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("  \\caption{Number of nodes and edges in call graphs computed using the " + algorithms + " algorithms.}")
      table.println("  \\label{table:Results}")
      table.println("\\end{table}")
      table.close
    }

    def emitTableBenchmarks = {
      val table = new PrintStream("table_benchmarks.tex")

      // Table Header
      table.println("\\begin{table}")
      table.println("\\centering")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * chatacteristics.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (chatacteristics.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        lazy val logfile = io.Source.fromFile(base + "/tca-expand-this/" + benchmark + "/" + log).getLines.toList

        row append s"\\textit{$benchmark}"

        // Read the stats log file
        emitBench(loc, nLoc)
        emitBench(classes, nClasses)
        emitBench(modules, nModules)
        emitBench(traits, nTraits)
        emitBench(mixins, nMixins)
        emitBench(methods, nMethods)
        emitBench(closures, nClosures)
        table.println(row append " \\\\")

        def emitBench(k: String, v: Int) = {
          val key = bench + " " + benchmark + " " + k
          val value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current benchmarks row
        }

        def extract(what: String) = logfile.find(_ contains what).get.split(":").last.trim.toInt
        lazy val nClasses = extract("# classes  ")
        lazy val nModules = extract("# objects  ")
        lazy val nTraits = extract("# traits  ")
        lazy val nMixins = extract("# trait compositions  ")
        lazy val nMethods = extract("# methods  ")
        lazy val nClosures = extract("# anonfun  ") + extract("# closures  ")
        lazy val nLoc = extract("# loc :")
      }

      // Table Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("  \\caption{Benchmark characteristics.}")
      table.println("  \\label{table:Benchmarks}")
      table.println("\\end{table}")
      table.close
    }

    def emitTableTimes = {
      val table = new PrintStream("table_time.tex")

      // Table Header
      table.println("\\begin{table}")
      table.println("\\centering")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * analyses.size) + "R" + "}")
      table.println("    \\toprule")
      table.println("    " + (analyses.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + s"& \\rot{\\textbf{$scalac}} " + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")

        row append s"\\textit{$benchmark}"

        // Read the time info
        emitTime(ra_all, time_ra_all)
        emitTime(ra_inst, time_ra_inst)
        emitTime(tca_bounds, time_tca_bounds)
        emitTime(tca_expand, time_tca_expand)
        emitTime(tca_expand_this, time_tca_expand_this)
        emitTime(wala_rta, time_wala_rta)
        emitTime(scalac, time_scalac)
        table.println(row append " \\\\")

        def emitTime(analysis: String, v: Float) = {
          val key = analysis + " " + benchmark + " " + time
          val value = floatFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row
        }

        def extractAnalysisTime(log: List[String]) = log.find(_ contains "Finished callgraph in").get.split(" ").dropRight(1).last.trim.toFloat
        lazy val time_ra_all = extractAnalysisTime(io.Source.fromFile(base + "/ra-all/" + benchmark + "/ra-all-log").getLines.toList)
        lazy val time_ra_inst = extractAnalysisTime(io.Source.fromFile(base + "/ra-inst/" + benchmark + "/ra-inst-log").getLines.toList)
        lazy val time_tca_bounds = extractAnalysisTime(io.Source.fromFile(base + "/tca-bounds/" + benchmark + "/tca-bounds-log").getLines.toList)
        lazy val time_tca_expand = extractAnalysisTime(io.Source.fromFile(base + "/tca-expand/" + benchmark + "/tca-expand-log").getLines.toList)
        lazy val time_tca_expand_this = extractAnalysisTime(io.Source.fromFile(base + "/tca-expand-this/" + benchmark + "/tca-expand-this-log").getLines.toList)
        lazy val time_wala_rta = io.Source.fromFile(base + "/wala-rta/" + benchmark + "/wala-rta-log").getLines.toList.find(_ contains "WALA took:").get.split(":").last.trim.toFloat
        lazy val time_scalac = {
          val line = io.Source.fromFile(base + "/scalac/" + benchmark + "/scalac-log").getLines.toList.find(_ contains "scalac.nowarn: finished").get
          Timer.round(line.split(" ").last.trim.drop(1).dropRight(3).toFloat / 1000.0).toFloat
        }
      }

      // Table Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("  \\caption{The time taken by the " + algorithms + " algorithms to compute the call graphs.}")
      table.println("  \\label{table:Time}")
      table.println("\\end{table}")
      table.close
    }
  }

}
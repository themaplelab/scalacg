package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.io.PrintStream
import java.text.DecimalFormat
import java.util.zip.GZIPInputStream

import scala.collection.JavaConversions.asScalaSet

import ca.uwaterloo.scalacg.util.Math
import probe.TextReader

object LatexGenerator {

  final val benchmarks = List("argot", "ensime", "fimpp", "joos", "kiama", "phantm", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")

  final val analyses = List("\\ra", "\\tcaNames", "\\tcaBounds", "\\tcaExpand", "\\tcaExpandThis", "\\rtaWala")
  final val algorithms = analyses.dropRight(1).mkString(", ") + ", and " + analyses.last

  final val chatacteristics = List("LOC") ++ List("classes", "objects", "traits", "trait comp.", "methods", "closures").map(a => s"\\texttt{\\#} $a")

  final val rq1Header = List("\\rtaWala~-~\\tcaBounds", "\\codett{apply}", "\\codett{toString}", "\\codett{equals}")

  final lazy val floatFormat = new DecimalFormat("#,###.##")
  final lazy val intFormat = "%,d"
  final lazy val perFormat = "%5s"

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
  final val tca_names = "tca names"
  final val ra = "ra"
  final val rta_wala = "rta wala"
  final val nodes = "nodes"
  final val edges = "edges"

  // keys for table of times
  final val time = "time"
  final val scalac = "scalac"

  // keys for table of differences
  final val rq1 = "rq1"
  final val valueKey = "value"
  final val perKey = "percentage"
  final val totalKey = "total"
  final val applyKey = "apply"
  final val toStringKey = "toString"
  final val equalsKey = "equals"

  final val base = "dist"

  final val sep = "\t"

  def main(args: Array[String]): Unit = {
    val data = new PrintStream("paper_data.tex")
    val out = Map[String, PrintStream](nodes -> new PrintStream(s"$nodes.csv"),
      edges -> new PrintStream(s"$edges.csv"),
      time -> new PrintStream(s"$time.csv"),
      rq1 -> new PrintStream(s"$rq1.csv"))

    // Emit latex files
    emitTableResults
    emitTableBenchmarks
    emitTableTimes
    emitTableRQ1

    data.close
    out.values foreach (_.close)

    def readCallGraph(file: String) = new TextReader().readCallGraph(new GZIPInputStream(new FileInputStream(file)))

    // Emit the results table with all the nodes, edges in it.
    def emitTableResults = {
      val table = new PrintStream("table_results.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Number of nodes and edges in the summarized version of call graphs computed using the " + algorithms + ".}")
      table.println("  \\label{table:results}")
      table.println("  \\begin{tabularx}{\\columnwidth}{ll" + ("R" * analyses.size) + "}")
      table.println("    \\toprule")
      table.println("    & " + (analyses.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + "\\\\")

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        // add benchmark name in italics
        row append s"\\multirow{2}{*}{\\$benchmark}"

        // Read the call graphs for this benchmark
        lazy val raCG = readCallGraph(s"$base/ra-all/$benchmark/$cg")
        lazy val tcaNamesCG = readCallGraph(s"$base/ra-inst/$benchmark/$cg")
        lazy val tcaBoundsCG = readCallGraph(s"$base/tca-bounds/$benchmark/$cg")
        lazy val tcaExpandCG = readCallGraph(s"$base/tca-expand/$benchmark/$cg")
        lazy val tcaExpandThisCG = readCallGraph(s"$base/tca-expand-this/$benchmark/$cg")
        lazy val rtaWalaCG = readCallGraph(s"$base/wala-rta/$benchmark/$walacg")

        // Emit nodes
        row append s" & $nodes"
        emit(ra, nodes, raCG.findReachables.size)
        emit(tca_names, nodes, tcaNamesCG.findReachables.size)
        emit(tca_bounds, nodes, tcaBoundsCG.findReachables.size)
        emit(tca_expand, nodes, tcaExpandCG.findReachables.size)
        emit(tca_expand_this, nodes, tcaExpandThisCG.findReachables.size)
        emit(rta_wala, nodes, rtaWalaCG.findReachables.size)
        table.println("    \\midrule")
        table.println(row append " \\\\")
        table.println("    \\cmidrule{2-8}")
        out(nodes).println(csv dropRight 1) // get rid of the last separator character
        row.clear
        csv.clear

        // Emit edges
        row append s" & $edges"
        emit(ra, edges, raCG.edgesIgnoringContext.size)
        emit(tca_names, edges, tcaNamesCG.edgesIgnoringContext.size)
        emit(tca_bounds, edges, tcaBoundsCG.edgesIgnoringContext.size)
        emit(tca_expand, edges, tcaExpandCG.edgesIgnoringContext.size)
        emit(tca_expand_this, edges, tcaExpandThisCG.edgesIgnoringContext.size)
        emit(rta_wala, edges, rtaWalaCG.edgesIgnoringContext.size)
        table.println(row append " \\\\")
        out(edges).println(csv dropRight 1) // get rid of the last separator character

        def emit(analysis: String, k: String, v: Int) = {
          val key = s"$analysis $benchmark $k"
          val value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row

          // print out to csv too
          csv append s"${value}${sep}"
        }
      }

      // Emit Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("\\end{table}")
      table.close
    }

    def emitTableBenchmarks = {
      val table = new PrintStream("table_benchmarks.tex")

      // Table Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Various characteristics of our benchmark programs.}")
      table.println("  \\label{table:benchmark:info}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * chatacteristics.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (chatacteristics.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        lazy val logfile = io.Source.fromFile(s"$base/tca-expand-this/$benchmark/$log").getLines.toList

        row append s"\\$benchmark"

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
          val key = s"$bench $benchmark $k"
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
      table.println("\\end{table}")
      table.close
    }

    def emitTableTimes = {
      val table = new PrintStream("table_time.tex")

      // Table Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{The time taken by " + algorithms + " to compute the call graphs.}")
      table.println("  \\label{table:results:time}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * analyses.size) + "R" + "}")
      table.println("    \\toprule")
      table.println("    " + (analyses.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + s"& \\rot{\\textbf{$scalac}} " + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        row append s"\\$benchmark"

        // Read the time info
        emitTime(ra, time_ra_all)
        emitTime(tca_names, time_ra_inst)
        emitTime(tca_bounds, time_tca_bounds)
        emitTime(tca_expand, time_tca_expand)
        emitTime(tca_expand_this, time_tca_expand_this)
        emitTime(rta_wala, time_wala_rta)
        emitTime(scalac, time_scalac)
        table.println(row append " \\\\")
        out(time).println(csv dropRight 1) // get rid of the last separator character

        def emitTime(analysis: String, v: Float) = {
          val key = s"$analysis $benchmark $time"
          val value = floatFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row

          // print out to csv too
          csv append s"${value}${sep}"
        }

        def extractAnalysisTime(log: List[String]) = log.find(_ contains "Finished callgraph in").get.split(" ").dropRight(1).last.trim.toFloat
        lazy val time_ra_all = extractAnalysisTime(io.Source.fromFile(s"$base/ra-all/$benchmark/ra-all-log").getLines.toList)
        lazy val time_ra_inst = extractAnalysisTime(io.Source.fromFile(s"$base/ra-inst/$benchmark/ra-inst-log").getLines.toList)
        lazy val time_tca_bounds = extractAnalysisTime(io.Source.fromFile(s"$base/tca-bounds/$benchmark/tca-bounds-log").getLines.toList)
        lazy val time_tca_expand = extractAnalysisTime(io.Source.fromFile(s"$base/tca-expand/$benchmark/tca-expand-log").getLines.toList)
        lazy val time_tca_expand_this = extractAnalysisTime(io.Source.fromFile(s"$base/tca-expand-this/$benchmark/tca-expand-this-log").getLines.toList)
        lazy val time_wala_rta = io.Source.fromFile(s"$base/wala-rta/$benchmark/wala-rta-log").getLines.toList.find(_ contains "WALA took:").get.split(":").last.trim.toFloat
        lazy val time_scalac = {
          val line = io.Source.fromFile(s"$base/scalac/$benchmark/scalac-log").getLines.toList.find(_ contains "scalac.nowarn: finished").get
          Math.round(line.split(" ").last.trim.drop(1).dropRight(3).toFloat / 1000.0).toFloat
        }
      }

      // Table Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("\\end{table}")
      table.close
    }

    def emitTableRQ1 = {
      val table = new PrintStream("table_rq1.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Comparison of precision between \\tcaBounds and \\rtaWala with respect to call edges.}")
      table.println("  \\label{table:benchmark:rq1}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("RL" * rq1Header.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (rq1Header.map(a => s"& \\multicolumn{2}{c}{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        import scala.collection.JavaConversions._

        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        // add benchmark name in italics
        row append s"\\$benchmark"

        // Read the call graphs for this benchmark
        lazy val rtaWalaCG = readCallGraph(s"$base/wala-rta/$benchmark/$walacg")
        lazy val tcaBoundsCG = readCallGraph(s"$base/tca-bounds/$benchmark/$cg")
        val rtaWalaEdges = rtaWalaCG.edgesIgnoringContext
        val tcaBoundsEdges = tcaBoundsCG.edgesIgnoringContext
        val diffEdges = rtaWalaEdges -- tcaBoundsEdges
        val totalDiff = diffEdges.size
        val applyDiff = diffEdges.filter(_.dst.name == "apply").size
        val toStringDiff = diffEdges.filter(_.dst.name == "toString").size
        val equalsDiff = diffEdges.filter(_.dst.name == "equals").size

        // Emit values
        emit(totalKey, totalDiff, rtaWalaEdges.size)
        emit(applyKey, applyDiff)
        emit(toStringKey, toStringDiff)
        emit(equalsKey, equalsDiff)

        table.println(row append " \\\\")
        out(rq1).println(csv dropRight 1) // get rid of the last separator character

        def emit(k: String, v: Int, t: Int = totalDiff) = {
          var key = s"rta wala tca bounds $benchmark nodes $valueKey $k"
          var value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}"
          csv append s"${value}${sep}"

          key = s"rta wala tca bounds $benchmark nodes $perKey $k"
          value = intFormat format Math.percentage(v, t)
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & (\\pgfkeysvalueof{$key}\\%)"
        }
      }

      // Table Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("\\end{table}")
      table.close
    }
  }

}
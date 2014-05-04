package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.io.PrintStream
import java.text.DecimalFormat
import java.util.zip.GZIPInputStream

import scala.collection.JavaConversions.asScalaSet

import ca.uwaterloo.scalacg.util.Math
import probe.TextReader

object LatexGenerator {
  final val benchmarks = List("argot", "ensime", "fimpp", "kiama", "phantm", "scalaxb", "scalisp", "see", "squeryl", "tictactoe")

  final val analyses = List("\\ra", "\\tcaNames", "\\tcaBounds", "\\tcaExpand", "\\tcaExpandThis", "\\rtaWala")
  final val analyses_cs = List("\\ra", "\\tcaExpandThis")
  final val algorithms = analyses.dropRight(1).mkString(", ") + ", and " + analyses.last

  final val characteristics = List("LOC") ++ List("classes", "objects", "traits", "trait;compositions", "methods",
    "closures", "call sites", "call sites on;abstract types", "call sites;on \\code{this}").map(a => s"\\texttt{\\#} $a")

  final val rq1Header = List("\\rtaWala~-~\\tcaBounds", "\\code{apply}", "\\code{toString}", "\\code{equals}")
  final val rq2Header = List("\\tcaNames~-~\\tcaBounds", "\\code{apply}")
  final val rq3Header = List("\\tcaBounds~-~\\tcaExpand") ++ List("abstract types").map(a => s"\\texttt{\\#} $a")
  final val rq4Header = List("\\tcaExpand~-~\\tcaExpandThis") ++ List("call sites", "eligible \\code{this} call sites").map(a => s"\\texttt{\\#} $a")

  final val analyses_csv = List("ra", "tca-names", "tca-bounds", "tca-expand", "tca-expand-this", "rta-wala")
  final val analyses_cs_csv = List("ra", "tca-expand-this")
  final val characteristics_csv = List("LOC", "# classes", "# objects", "# traits", "# trait compositions", "# methods",
    "# closures", "# call sites", "# call sites on abstract types", "# call sites on this")
  final val rq1Header_csv = List("rta-wala - tca-bounds", "apply", "toString", "equals")
  final val rq2Header_csv = List("tca-names - tca-bounds", "apply")
  final val rq3Header_csv = List("tca-bounds - tca-expand", "#abstract types")
  final val rq4Header_csv = List("tca-expand - tca-expand-this", "#call sites", "#eligible this call sites")

  final lazy val floatFormat = new DecimalFormat("#,###.#")
  final lazy val intFormat = "%,d"
  final lazy val perFormat = "%5s"

  // constant file names
  final val cg = "callgraph-summary.txt.gzip"
  final val walacg = "wala-" + cg
  final val log = "tca-expand-log"

  // keys for table of characteristics
  final val bench = "benchmark"
  final val loc = "LOC"
  final val classes = "classes"
  final val modules = "modules"
  final val traits = "traits"
  final val mixins = "mixins"
  final val methods = "methods"
  final val closures = "closures" // these include anonfun
  final val totalCallSites = "total call sites"
  final val callSitesAbstract = "call sites on abstract types"
  final val callSitesThis = "call sites on this"

  // keys for table of analyses
  final val tca_expand_this = "tca expand this"
  final val tca_expand = "tca expand"
  final val tca_bounds = "tca bounds"
  final val tca_names = "tca names"
  final val ra = "ra"
  final val rta_wala = "rta wala"
  final val nodes = "nodes"
  final val edges = "edges"
  final val callsites = "callsites"
  final val monoKey = "mono"
  final val polyKey = "poly"
  final val reachableKey = "reachable"

  // keys for table of times
  final val time = "time"
  final val scalac = "scalac"

  // keys for table of differences
  final val rq1 = "rq1"
  final val rq2 = "rq2"
  final val rq3 = "rq3"
  final val rq4 = "rq4"
  final val valueKey = "value"
  final val perKey = "percentage"
  final val totalKey = "total"
  final val applyKey = "apply"
  final val toStringKey = "toString"
  final val equalsKey = "equals"
  final val absTpeKey = "abstract types"
  final val csKey = "call sites"
  final val thisKey = "this"
  final val superKey = "super"

  final val base = "dist"

  final val sep = "\t"

  def doubleLines(str: String) = {
    val tokens = str.split(';') // should yield 1 or 2 tokens exactly
    if (tokens.size == 1) s"\\rot{\\textbf{$str}}" // one line
    else if (tokens.size == 2) s"\\rot{\\textbf{${tokens(0)}}} \\rot{\\textbf{${tokens(1)}}}" // two lines
    else throw new RuntimeException("more than 2 lines is not allowed.")
  }

  def main(args: Array[String]): Unit = {
    val data = new PrintStream("tex/paper_data.tex")
    val out = Map[String, PrintStream](nodes -> new PrintStream(s"csv/$nodes.csv"),
      edges -> new PrintStream(s"csv/$edges.csv"),
      callsites -> new PrintStream(s"csv/$callsites.csv"),
      time -> new PrintStream(s"csv/$time.csv"),
      bench -> new PrintStream(s"csv/$bench.csv"))

    // Emit latex files
    // TODO: select the tables to emit
    //    emitTableResults
    emitTableCallsites
    //    emitTableBenchmarks
    //    emitTableTimes
    // emitTableRQ1
    // emitTableRQ2
    // emitTableRQ3
    // emitTableRQ4

    data.close
    out.values foreach (_.close)

    def readCallGraph(file: String) = new TextReader().readCallGraph(new GZIPInputStream(new FileInputStream(file)))

    // Emit the results table with all the nodes, edges in it.
    def emitTableResults = {
      val table = new PrintStream("tex/table_results.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Number of nodes and edges in the summarized version of call graphs computed using the " + algorithms + ".}")
      table.println("  \\label{table:results}")
      table.println("  \\begin{tabularx}{\\columnwidth}{ll" + ("R" * analyses.size) + "}")
      table.println("    \\toprule")
      table.println("    & " + (analyses.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + "\\\\")

      out(nodes).println("program" + sep + analyses_csv.mkString(sep))
      out(edges).println("program" + sep + analyses_csv.mkString(sep))

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        // add benchmark name in italics
        row append s"\\multirow{2}{*}{\\$benchmark}"
        csv append benchmark append sep

        // Read the call graphs for this benchmark
        lazy val raCG = readCallGraph(s"$base/ra/$benchmark/$cg")
        lazy val tcaNamesCG = readCallGraph(s"$base/tca-names/$benchmark/$cg")
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
        csv append benchmark append sep
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

    // Emit the results table with the callsite stats in it.
    def emitTableCallsites = {
      val table = new PrintStream("tex/table_callsites.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Number of reachable, monomorphic, and polymorphic call sites in the summarized version of call graphs computed using the \\ra, and \\tcaExpandThis.}")
      table.println("  \\label{table:callsites}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * 3 * analyses_cs.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (analyses_cs.map(a => s"& \\multicolumn{3}{c}{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    " + (analyses_cs.map(e => analyses_cs.indexOf(e) * 3).map(a => s"\\cmidrule(lr){${2 + a}-${4 + a}} ").mkString) + "\\\\")
      table.println("	 & \\textbf{Total} & \\textbf{Mono} & \\textbf{Poly} & \\textbf{Total} & \\textbf{Mono} & \\textbf{Poly} \\\\")
      table.println("    " + (analyses_cs.map(e => analyses_cs.indexOf(e) * 3).map(a => s"\\cmidrule(lr){${2 + a}-${4 + a}} ").mkString) + "\\\\")

      out(callsites).println("program" + sep + analyses_cs_csv.mkString(sep))
      out(callsites).println("" + sep + totalKey + sep + monoKey + sep + polyKey)

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        // add benchmark name in italics
        row append s"\\$benchmark"
        csv append benchmark append sep

        // Emit nodes
        // Read the time info
        emitCallSites("ra")
        emitCallSites("tca-expand-this")
        table.println(row append " \\\\")
        out(time).println(csv dropRight 1) // get rid of the last separator character

        def emitCallSites(analysis: String) = {
          emit(analysis, totalKey, total(analysis))
          emit(analysis, monoKey, mono(analysis))
          emit(analysis, polyKey, poly(analysis))
        }

        def emit(analysis: String, k: String, v: Int) = {
          val key = s"$analysis $benchmark $k $reachableKey $callsites"
          val value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current results row

          // print out to csv too
          csv append s"${value}${sep}"
        }

        def extract(analysis: String, what: String) = io.Source.fromFile(s"$base/$analysis/$benchmark/$analysis-log").getLines.toList.find(_ contains what).get.split(":").last.trim.toInt
        def total(analysis: String) = extract(analysis, "# reachable call sites")
        def mono(analysis: String) = extract(analysis, "# monomorphic call sites")
        def poly(analysis: String) = extract(analysis, "# polymorphic call sites")
      }

      // Emit Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("\\end{table}")
      table.close
    }

    def emitTableBenchmarks = {
      val table = new PrintStream("tex/table_benchmarks.tex")

      // Table Header
      table.println("\\begin{table}[!b]")
      table.println("\\centering")
      table.println("  \\caption{Various characteristics of our benchmark programs.}")
      table.println("  \\label{table:benchmark:info}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * characteristics.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (characteristics.map(a => s"& ${doubleLines(a)} ").mkString) + "\\\\")
      table.println("    \\midrule")

      out(bench).println("program" + sep + characteristics_csv.mkString(sep))

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")
        lazy val logfile = io.Source.fromFile(s"$base/tca-expand/$benchmark/$log").getLines.toList

        row append s"\\$benchmark"
        csv append benchmark append sep

        // Read the stats log file
        emitBench(loc, nLoc)
        emitBench(classes, nClasses)
        emitBench(modules, nModules)
        emitBench(traits, nTraits)
        emitBench(mixins, nMixins)
        emitBench(methods, nMethods)
        emitBench(closures, nClosures)
        emitBench(totalCallSites, nTotalCallSites)
        emitBench(callSitesAbstract, nCallSitesAbstract)
        emitBench(callSitesThis, nCallSitesThis)
        table.println(row append " \\\\")
        out(bench).println(csv dropRight 1) // get rid of the last separator character

        def emitBench(k: String, v: Int) = {
          val key = s"$bench $benchmark $k"
          val value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}" // add the key to the current benchmarks row

          // print out to csv too
          csv append s"${value}${sep}"
        }

        def extract(what: String) = logfile.find(_ contains what).get.split(":").last.trim.toInt
        lazy val nClasses = extract("# classes  ")
        lazy val nModules = extract("# objects  ")
        lazy val nTraits = extract("# traits  ")
        lazy val nMixins = extract("# trait compositions  ")
        lazy val nMethods = extract("# methods  ")
        lazy val nClosures = extract("# anonfun  ") + extract("# closures  ")
        lazy val nLoc = extract("# loc :")
        lazy val nTotalCallSites = extract("# total call sites")
        lazy val nCallSitesAbstract = extract("# call sites on abs types")
        lazy val nCallSitesThis = extract("# call sites on this")
      }

      // Table Footer
      table.println("    \\bottomrule")
      table.println("  \\end{tabularx}")
      table.println("\\end{table}")
      table.close
    }

    def emitTableTimes = {
      val table = new PrintStream("tex/table_time.tex")

      // Table Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{The time (in seconds) taken by " + algorithms + " to compute the call graphs.}")
      table.println("  \\label{table:results:time}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("R" * analyses.size) + "R" + "}")
      table.println("    \\toprule")
      table.println("    " + (analyses.map(a => s"& \\rot{\\textbf{$a}} ").mkString) + s"& \\rot{\\textbf{$scalac}} " + "\\\\")
      table.println("    \\midrule")

      out(time).println("program" + sep + analyses_csv.mkString(sep) + sep + scalac)

      for (benchmark <- benchmarks) {
        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        row append s"\\$benchmark"
        csv append benchmark append sep

        // Read the time info
        emitTime(ra, time_ra)
        emitTime(tca_names, time_tca_name)
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
        lazy val time_ra = extractAnalysisTime(io.Source.fromFile(s"$base/ra/$benchmark/ra-log").getLines.toList)
        lazy val time_tca_name = extractAnalysisTime(io.Source.fromFile(s"$base/tca-names/$benchmark/tca-names-log").getLines.toList)
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
      val table = new PrintStream("tex/table_rq1.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Comparison of precision between \\rtaWala and \\tcaBounds with respect to call edges.}")
      table.println("  \\label{table:results:rq1}")
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
          var key = s"$rq1 $benchmark $k $valueKey"
          var value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}"
          csv append s"${value}${sep}"

          key = s"$rq1 $benchmark $k $perKey"
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

    def emitTableRQ2 = {
      val table = new PrintStream("tex/table_rq2.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Comparison of precision between \\tcaBounds and \\tcaNames with respect to call edges.}")
      table.println("  \\label{table:results:rq2}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("RL" * rq2Header.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (rq2Header.map(a => s"& \\multicolumn{2}{c}{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        import scala.collection.JavaConversions._

        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")

        // add benchmark name in italics
        row append s"\\$benchmark"

        // Read the call graphs for this benchmark
        lazy val tcaBoundsCG = readCallGraph(s"$base/tca-bounds/$benchmark/$cg")
        lazy val tcaNamesCG = readCallGraph(s"$base/tca-names/$benchmark/$cg")
        val tcaBoundsEdges = tcaBoundsCG.edgesIgnoringContext
        val tcaNamesEdges = tcaNamesCG.edgesIgnoringContext
        val diffEdges = tcaNamesEdges -- tcaBoundsEdges
        val totalDiff = diffEdges.size
        val applyDiff = diffEdges.filter(_.dst.name == "apply").size

        // Emit values
        emit(totalKey, totalDiff, tcaNamesEdges.size)
        emit(applyKey, applyDiff)

        table.println(row append " \\\\")
        out(rq2).println(csv dropRight 1) // get rid of the last separator character

        def emit(k: String, v: Int, t: Int = totalDiff) = {
          var key = s"$rq2 $benchmark $k $valueKey"
          var value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}"
          csv append s"${value}${sep}"

          key = s"$rq2 $benchmark $k $perKey"
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

    def emitTableRQ3 = {
      val table = new PrintStream("tex/table_rq3.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Comparison of precision between \\tcaExpand and \\tcaBounds with respect to call edges.}")
      table.println("  \\label{table:results:rq3}")
      table.println("  \\begin{tabularx}{\\columnwidth}{l" + ("RL" * rq3Header.size) + "}")
      table.println("    \\toprule")
      table.println("    " + (rq3Header.map(a => s"& \\multicolumn{2}{c}{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        import scala.collection.JavaConversions._

        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")
        lazy val logfile = io.Source.fromFile(s"$base/tca-expand/$benchmark/$log").getLines.toList

        // add benchmark name in italics
        row append s"\\$benchmark"

        // Read the call graphs for this benchmark
        lazy val tcaBoundsCG = readCallGraph(s"$base/tca-bounds/$benchmark/$cg")
        lazy val tcaExpandCG = readCallGraph(s"$base/tca-expand/$benchmark/$cg")
        val tcaBoundsEdges = tcaBoundsCG.edgesIgnoringContext
        val tcaExpandEdges = tcaExpandCG.edgesIgnoringContext
        val diffEdges = tcaBoundsEdges -- tcaExpandEdges
        val totalDiff = diffEdges.size

        // Emit values
        emit(edges, totalDiff, tcaBoundsEdges.size)
        emit(absTpeKey, absClasses, totClasses)

        table.println(row append " \\\\")
        out(rq3).println(csv dropRight 1) // get rid of the last separator character

        def extract(what: String) = logfile.find(_ contains what).get.split(":").last.trim.toInt
        lazy val absClasses = extract("# classes w abs type member") + extract("# classes w abs type param")
        lazy val totClasses = extract("# classes  ") + extract("# objects  ") + extract("# traits  ") + extract("# anonfun  ")

        def emit(k: String, v: Int, t: Int = totalDiff) = {
          var key = s"$rq3 $benchmark $k $valueKey"
          var value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}"
          csv append s"${value}${sep}"

          key = s"$rq3 $benchmark $k $perKey"
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

    def emitTableRQ4 = {
      val table = new PrintStream("tex/table_rq4.tex")

      // Emit Header
      table.println("\\begin{table}[!t]")
      table.println("\\centering")
      table.println("  \\caption{Comparison of precision between \\tcaExpandThis and \\tcaExpand with respect to call edges.}")
      table.println("  \\label{table:results:rq4}")
      table.println("  \\begin{tabularx}{\\columnwidth}{lRLR" + ("RL" * rq4Header.tail.tail.size) + "}")
      table.println("    \\toprule")
      table.println(s"    & \\multicolumn{2}{c}{\\textbf{${rq4Header.head}}} & \\textbf{${rq4Header.tail.head}} " + (rq4Header.tail.tail.map(a => s"& \\multicolumn{2}{c}{\\textbf{$a}} ").mkString) + "\\\\")
      table.println("    \\midrule")

      for (benchmark <- benchmarks) {
        import scala.collection.JavaConversions._

        var row = new StringBuilder("    ")
        var csv = new StringBuilder("")
        lazy val logfile = io.Source.fromFile(s"$base/tca-expand/$benchmark/$log").getLines.toList

        // add benchmark name in italics
        row append s"\\$benchmark"

        // Read the call graphs for this benchmark
        lazy val tcaExpandThisCG = readCallGraph(s"$base/tca-expand-this/$benchmark/$cg")
        lazy val tcaExpandCG = readCallGraph(s"$base/tca-expand/$benchmark/$cg")
        val tcaExpandThisEdges = tcaExpandThisCG.edgesIgnoringContext
        val tcaExpandEdges = tcaExpandCG.edgesIgnoringContext
        val diffEdges = tcaExpandEdges -- tcaExpandThisEdges
        val totalDiff = diffEdges.size

        // Emit values
        emit(edges, totalDiff, tcaExpandEdges.size)
        emitValue(csKey, totalCS)
        emit(s"$thisKey $csKey", thisCS, totalCS)

        table.println(row append " \\\\")
        out(rq4).println(csv dropRight 1) // get rid of the last separator character

        def extract(what: String) = logfile.find(_ contains what).get.split(":").last.trim.toInt
        lazy val totalCS = extract("# total call sites")
        lazy val thisCS = extract("# call sites on this")

        def emitValue(k: String, v: Int) = {
          val key = s"$rq4 $benchmark $k $valueKey"
          val value = intFormat format v
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & \\pgfkeysvalueof{$key}"
          csv append s"${value}${sep}"
        }

        def emitPercentage(k: String, v: Int, t: Int = totalDiff) = {
          val key = s"$rq4 $benchmark $k $perKey"
          val value = intFormat format Math.percentage(v, t)
          data.println(s"\\pgfkeyssetvalue{$key}{$value}")
          row append s" & (\\pgfkeysvalueof{$key}\\%)"
        }

        def emit(k: String, v: Int, t: Int = totalDiff) = {
          emitValue(k, v)
          emitPercentage(k, v, t)
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
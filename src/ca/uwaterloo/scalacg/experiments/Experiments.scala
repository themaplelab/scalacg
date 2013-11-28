package ca.uwaterloo.scalacg.experiments

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.collection.mutable.Map
import scala.collection.mutable.{ Set => MutableSet }
import scala.reflect.io.File

import probe.CallEdge
import probe.GXLReader
import probe.ProbeMethod

object Experiments {
  //  final lazy val benchmarks = List("argot", "fimpp", "joos", "kiama", "phantm", "scalisp", "see", "tictactoe")
  final lazy val benchmarks = List("tictactoe")

  def main(args: Array[String]) = {
    var prefix = ""
    var experiment = ""

    // Parse the arguments
    args.length match {
      case 0 =>
        prefix = "../scalabench/local/dist/"
        experiment = "tca-wala"
      case 2 =>
        prefix = args(0)
        experiment = args(1)
      case _ =>
        throw new IllegalArgumentException("Wrong number of arguments.")
    }

    lazy val tca_wala = new Experiment("tca-wala", prefix)("tca-this-super", "callgraph-summary.gxl.gzip")("wala", "wala-callgraph-summary.gxl.gzip")
    lazy val this_nothis = new Experiment("this-nothis", prefix)("tca-this-super", "callgraph.gxl.gzip")("tca-super", "callgraph.gxl.gzip")
    lazy val tca_ra = new Experiment("tca-ra", prefix)("tca-this-super", "callgraph.gxl.gzip")("ra", "callgraph.gxl.gzip")
    lazy val tca_dyn = new Experiment("tca-dyn", prefix)("tca-this-super", "callgraph.gxl.gzip")("dyn", "callgraph.gxl.gzip")

    experiment match {
      case "tca-wala" => tca_wala.print
      case "this-nothis" => this_nothis.print
      case "tca-ra" => tca_ra.print
      case "tca-dynamic" => tca_dyn.print
      case _ => throw new IllegalArgumentException("Uknown experiment requested")
    }
  }

  // Implicitly converts a Java parameterless Set to a parameterized Scala set.
  implicit def asScalaSet[A](set: java.util.Set[_]): Set[A] = {
    var ret = Set.empty[A]
    val iter = set.iterator
    while (iter.hasNext) {
      ret += iter.next.asInstanceOf[A]
    }
    ret
  }

  class Experiment(name: String, prefix: String)(dirA: String, cgA: String)(dirB: String, cgB: String) {
    lazy val reachables = new Stat
    lazy val edges = new Stat
    lazy val types = new Stat

    benchmarks.foreach { benchmark =>
      // Get the call graphs
      lazy val pathA = prefix + dirA + File.separator + benchmark + File.separator + cgA
      lazy val pathB = prefix + dirB + File.separator + benchmark + File.separator + cgB
      lazy val streamA = new GZIPInputStream(new FileInputStream(pathA))
      lazy val streamB = new GZIPInputStream(new FileInputStream(pathB))
      lazy val supergraph = new GXLReader().readCallGraph(streamA)
      lazy val subgraph = new GXLReader().readCallGraph(streamB)

      // Get the set of instantiated types
      lazy val typesA = getTypes(prefix + dirA + File.separator + benchmark + File.separator + "instantiated.txt")
      lazy val typesB = getTypes(prefix + dirB + File.separator + benchmark + File.separator + "instantiated.txt")

      if (name != "tca-wala") {
        types.a(benchmark) = typesA.size
        types.b(benchmark) = typesB.size
        types.a_b(benchmark) = (typesA -- typesB).size
        types.b_a(benchmark) = (typesB -- typesA).size
      }

      // Reachable methods
      val reachA: Set[ProbeMethod] = supergraph.findReachables
      val reachB: Set[ProbeMethod] = subgraph.findReachables

      reachables.a(benchmark) = reachA.size
      reachables.b(benchmark) = reachB.size
      reachables.a_b(benchmark) = (reachA -- reachB).size
      reachables.b_a(benchmark) = (reachB -- reachA).size

      // Edges
      val edgesA: Set[CallEdge] = supergraph.edges
      val edgesB: Set[CallEdge] = subgraph.edges

      //            (reachA -- reachB).toSeq.sortWith((a, b) => a.name < b.name).foreach(println)
      //            println("===========================================================================")
      (edgesB -- edgesA).toSeq.sortWith((a, b) => a.src.name < b.src.name).foreach(println)
      //            if (name != "tca-wala") {
      //              println("===========================================================================")
      //              (typesA -- typesB).toSeq.sorted.foreach(println)
      //            }
      println("\n")

      edges.a(benchmark) = edgesA.size
      edges.b(benchmark) = edgesB.size
      edges.a_b(benchmark) = (edgesA -- edgesB).size
      edges.b_a(benchmark) = (edgesB -- edgesA).size

      // Close streams
      streamA.close
      streamB.close
    }

    def print = {
      val reach = "Reachable Methods"
      val eds = "Call Edges"
      val tps = "Instantiated Types"

      lazy val format = ("%16s" * benchmarks.size) + "%n"
      val reach_t = " " * 16
      val sup = name.split("-").head.toUpperCase
      val sub = name.split("-").reverse.head.toUpperCase
      val sup_t = " " * (reach_t.length - sup.length)
      val sub_t = " " * (reach_t.length - sub.length)

      val sup_sub = sup + " - " + sub
      val sub_sup = sub + " - " + sup
      val sup_sub_t = " " * (reach_t.length - sup_sub.length)

      println("=" * name.length)
      println(name.toUpperCase)
      println("=" * name.length)

      // Benchmarks table header
      printf(reach_t + format, benchmarks: _*)
      printf(reach_t + format, benchmarks.map(b => "-" * b.length): _*)

      // Reachable methods
      println(reach)
      println("=" * reach.length)
      printf(sup + sup_t + format, reachables.a.toSeq.sorted.map(_._2): _*)
      printf(sub + sub_t + format, reachables.b.toSeq.sorted.map(_._2): _*)
      printf(sup_sub + sup_sub_t + format, reachables.a_b.toSeq.sorted.map(_._2): _*)
      printf(sub_sup + sup_sub_t + format, reachables.b_a.toSeq.sorted.map(_._2): _*)
      println

      // Edges
      println(eds)
      println("=" * eds.length)
      printf(sup + sup_t + format, edges.a.toSeq.sorted.map(_._2): _*)
      printf(sub + sub_t + format, edges.b.toSeq.sorted.map(_._2): _*)
      printf(sup_sub + sup_sub_t + format, edges.a_b.toSeq.sorted.map(_._2): _*)
      printf(sub_sup + sup_sub_t + format, edges.b_a.toSeq.sorted.map(_._2): _*)
      println

      // Types
      if (name != "tca-wala") {
        println(tps)
        println("=" * tps.length)
        printf(sup + sup_t + format, types.a.toSeq.sorted.map(_._2): _*)
        printf(sub + sub_t + format, types.b.toSeq.sorted.map(_._2): _*)
        printf(sup_sub + sup_sub_t + format, types.a_b.toSeq.sorted.map(_._2): _*)
        printf(sub_sup + sup_sub_t + format, types.b_a.toSeq.sorted.map(_._2): _*)
        println
      }

      println
    }

    private def getTypes(file: String) = {
      val result = MutableSet[String]()
      for (line <- io.Source.fromFile(file).getLines) {
        result += line
      }
      result
    }
  }

  final class Stat {
    // Values
    val a = Map[String, Int]().withDefaultValue(0)
    val b = Map[String, Int]().withDefaultValue(0)

    // Comparisons
    val a_b = Map[String, Int]().withDefaultValue(0)
    val b_a = Map[String, Int]().withDefaultValue(0)
  }
}
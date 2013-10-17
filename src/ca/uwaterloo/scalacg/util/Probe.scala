package ca.uwaterloo.scalacg.util

import java.io.PrintStream
import scala.collection.mutable.StringBuilder
import scala.reflect.io.AbstractFile
import ca.uwaterloo.scalacg.analysis.CallGraphAnalysis
import ca.uwaterloo.scalacg.config.Global
import ca.uwaterloo.scalacg.probe.CallEdge
import ca.uwaterloo.scalacg.probe.CallSiteContext
import ca.uwaterloo.scalacg.probe.GXLWriter
import probe.CallGraph
import probe.ObjectManager
import probe.ProbeMethod
import java.util.zip.GZIPOutputStream

trait Probe extends Global {

  import global._

  class StringHelper(str: String) {
    def replaceLast(regex: String, replacement: String) = str.reverse.replaceFirst(regex, replacement.reverse).reverse
  }
  implicit def stringWrapper(string: String) = new StringHelper(string)

  /**
   * A printable name for a that uses the probe signature, surround by "<" and ">", useful when printing sets of methods.
   */
  def signature(method: Symbol) = {
    "<" + probeMethod(method) + ">"
  }

  /**
   * Get a probe method for the given symbol
   */
  def probeMethod(methodSymbol: Symbol): ProbeMethod = {
    val probeClass = ObjectManager.v().getClass(effectiveOwnerName(methodSymbol))
    val probeMethod = ObjectManager.v().getMethod(probeClass, methodSymbol.simpleName.decode, paramsSignatureString(methodSymbol))
    probeMethod
  }

  /**
   * Get the full name (dot separated) of the owner of a method symbol. That acts like the method declaring class in Soot.
   */
  def effectiveOwnerName(methodSymbol: Symbol): String = {
    methodSymbol.fullName.replace("." + methodSymbol.simpleName, "")
  }

  /**
   * Get the params string of a method symbol
   */
  def paramsSignatureString(methodSymbol: Symbol): String = {
    methodSymbol.signatureString.substring(0, methodSymbol.signatureString.indexOf(')') + 1).replace("(", "").replace(")", "")
  }

  /**
   * Return zero if x is negative.
   */
  def getOrZero(x: Int) = {
    x match {
      case n if n < 0 => 0
      case n => n
    }
  }
}

trait CallGraphPrinter {
  self: Probe with CallGraphAnalysis with Annotations =>

  /**
   * Return a probe call graph in GXL format.
   */
  def printProbeCallGraph = {
    println("Call graph is available at callgraph.gxl.gzip")
    val out = new PrintStream("callgraph.gxl.gzip")
    val probeCallGraph = new CallGraph
    val entryPointsOut = new PrintStream("entrypoints.txt")
    val libraryOut = new PrintStream("library.txt")

    // Get the entry points (these include main methods and call-backs)
    for {
      entry <- entryPoints ++ callBacks
    } {
      probeCallGraph.entryPoints.add(probeMethod(entry))
      entryPointsOut.println(methodToId.getOrElse(entry, 0) + " ===> " + probeMethod(entry))
    }
    entryPointsOut.close

    // Get the edges
    for {
      callSite <- callGraph.keys
      source = callSite.enclMethod
      target <- callGraph(callSite)
      sourceFile = if (callSite.position.isDefined) relativize(callSite.position.source.file) else "unknown"
      line = if (callSite.position.isDefined) callSite.position.line.toString else "-1"
    } {
      val edge = new CallEdge(probeMethod(source), probeMethod(target), new CallSiteContext(sourceFile + " : line " + line))
      probeCallGraph.edges.add(edge)

      // Print out library methods to be added to the WALA call graph
      if (isLibrary(target)) libraryOut.println(edge.src + " ===> " + edge.dst)
    }
    libraryOut.close

    // Write GXL file in gzip format to save space.
    new GXLWriter().write(probeCallGraph, new GZIPOutputStream(out))
  }
  
  /**
   * Print the mapping of all annotated methods to their source level signature.
   */
  def printMethods = {
    val out = new PrintStream("methods.txt")
    for (method <- methodToId.keys) {
      out.println(methodToId.getOrElse(method, 0) + " ===> " + probeMethod(method))
    }
  }

  /**
   * Get the prefix of the output files based on the plugin options.
   */
  lazy val prefix = {
    val ret = new StringBuilder
    ret ++= pluginOptions.analysis.toString.toLowerCase + "-"
    if (pluginOptions.doThis) ret ++= "this-"
    if (pluginOptions.doSuperCalls) ret ++= "super-"
    if (pluginOptions.doAssertions) ret ++= "assert-"
    ret.toString
  }

  /**
   * Get the relative path for an absolute source file path.
   */
  private def relativize(file: AbstractFile) = {
    file.toString().replaceFirst(".+/build_src/[^/]+/", "")
  }

  /**
   * Get the relative path for an absolute source file path.
   */
  private def relativize(absolute: String) = {
    absolute.replaceFirst(".+/build_src/[^/]+/", "")
  }
}


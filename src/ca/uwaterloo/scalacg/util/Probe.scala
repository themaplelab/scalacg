package ca.uwaterloo.scalacg.util

import java.io.PrintStream
import java.util.zip.GZIPOutputStream

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

trait Probe extends Global {

  import global._

  /**
   * A printable name for a that uses the probe signature, surround by "<" and ">", useful when printing sets of methods.
   */
  def signature(method: Symbol) = {
    "<" + probeMethod(method).correctToString + ">"
  }

  /**
   * Get a probe method for the given symbol
   */
  def probeMethod(methodSymbol: Symbol): ProbeMethod = {
    //    println(methodSymbol.fullName + " ::" + methodSymbol.pkg + "::" + methodSymbol.cls + "::" + methodSymbol.nme)
    val probeClass = ObjectManager.v().getClass(methodSymbol.pkg, methodSymbol.cls)
    val probeMethod = ObjectManager.v().getMethod(probeClass, methodSymbol.nme, methodSymbol.paramsSig)
    //    println(probeMethod.correctToString)
    probeMethod
  }

  lazy val libraryBlob = {
    val cls = ObjectManager.v().getClass("ca.uwaterloo.scalacg.Library")
    ObjectManager.v().getMethod(cls, "blob", "")
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
    println("Call graph is available at callgraph.gxl.gzip and its summary at callgraph-summary.gxl.gzip")
    val out = new PrintStream("callgraph.gxl.gzip")
    val probeCallGraph = new CallGraph

    val summary = new PrintStream("callgraph-summary.gxl.gzip")
    val probeSummary = new CallGraph

    val entryPointsOut = new PrintStream("entrypoints.txt")
    val libraryOut = new PrintStream("library.txt")
    val callbacksOut = new PrintStream("callbacks.txt")

    // Get the entry points (these include main methods, and module constructors)
    for {
      entry <- entryPoints ++ moduleConstructors
    } {
      val method = probeMethod(entry)
      probeCallGraph.entryPoints.add(method)
      if (isApplication(entry)) probeSummary.entryPoints.add(method)
      entryPointsOut.println(methodToId.getOrElse(entry, 0) + " ===> " + method.show)
    }

    // Call backs originate from the library blob in the summary call graph but entry points in the regular one
    for {
      callback <- callBacks
    } {
      val callbackMethod = probeMethod(callback)
      probeCallGraph.entryPoints.add(callbackMethod)
      if (isApplication(callback)) probeSummary.edges.add(new CallEdge(libraryBlob, callbackMethod, unknownContext))

      val e = methodToId.getOrElse(callback, 0) + " ===> " + callbackMethod.show
      callbacksOut.println(e)
      entryPointsOut.println(e)
    }

    // Get the edges
    for {
      callSite <- callGraph.keys
      source = callSite.enclMethod
      target <- callGraph(callSite)
      sourceFile = if (callSite.position.isDefined) relativize(callSite.position.source.file) else "unknown"
      line = if (callSite.position.isDefined) callSite.position.line.toString else "-1"
      sourceMethod = probeMethod(source)
      targetMethod = probeMethod(target)
      context = new CallSiteContext(sourceFile + " : line " + line)
    } {
      val edge = new CallEdge(sourceMethod, targetMethod, context)
      probeCallGraph.edges.add(edge)

      val isSourceApp = isApplication(source)
      val isTargetApp = isApplication(target)

      //      if (sourceMethod.name == "apply" && targetMethod.name == "paramString") {
      //        println(sourceMethod + " :: " + isSourceApp + " :: " + getPackageName(source) + " :: " + packageNames.contains(getPackageName(source)))
      //        println(targetMethod + " :: " + isTargetApp + " :: " + getPackageName(target) + " :: " + packageNames.contains(getPackageName(target)))
      //      }

      // Print out library methods to be added to the WALA call graph
      if (!isTargetApp) libraryOut.println(sourceMethod.show + " ===> " + targetMethod.show)

      // Now put the edge in the summary call graph
      if (isSourceApp && isTargetApp) {
        val edge = new CallEdge(sourceMethod, targetMethod, context)
        probeSummary.edges.add(edge)
      } else if (isSourceApp && !isTargetApp) {
        val edge = new CallEdge(sourceMethod, libraryBlob, context)
        probeSummary.edges.add(edge)
      } else {
        throw new UnsupportedOperationException("source method cannot be in the library: " + sourceMethod)
      }
    }

    // Close the streams
    callbacksOut.close
    entryPointsOut.close
    libraryOut.close

    // Write GXL file in gzip format to save space.
    new GXLWriter().write(probeCallGraph, new GZIPOutputStream(out))
    new GXLWriter().write(probeSummary, new GZIPOutputStream(summary))
  }

  lazy val unknownContext = new CallSiteContext("unknown : -1")

  /**
   * Print the mapping of all annotated methods to their source level signature.
   */
  def printMethods = {
    val out = new PrintStream("methods.txt")
    for (method <- methodToId.keys) {
      out.println(methodToId.getOrElse(method, 0) + " ===> " + probeMethod(method).show)
    }
  }

  /**
   * Print the set of instantiated types
   */
  def printInstantiatedTypes = {
    val out = new PrintStream("instantiated.txt")
    for (tpe <- instantiatedTypes.reachableItems) {
      out.println(tpe.baseClasses.map(_.fullName).mkString("\t"))
    }
  }

  /**
   * Print the names of application packages to be used later by WALA.
   */
  def printPackageNames = {
    val out = new PrintStream("packages.txt")
    for (pkg <- packageNames) {
      out.println(pkg)
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


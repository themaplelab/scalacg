package callgraph.util

import callgraph.AbstractAnalysis
import ca.uwaterloo.scalacg.util.CGAnnotations
import probe.CallGraph
import java.io.PrintStream
import scalacg.probe.{GXLWriter, CallSiteContext, CallEdge}
import reflect.io.AbstractFile

trait CGPrint {

  this: AbstractAnalysis with LibraryCalls with CGAnnotations =>

  import global._

  def printAnnotatedCallsites() {
    printTargets()
    printInvocations()

    def printTargets() {
      for {
        callSite <- callSites
        if reachableCode contains callSite.enclMethod
        expected = callSite.annotation.toSet
        if expected.nonEmpty
      } {
        println(callSite.staticTarget + " " + callSite.annotation)

        val resolved = callGraph(callSite).map(findTargetAnnotation)
        printCallGraph(resolved, isResolved = true)
        printCallGraph(expected, isResolved = false)
        assert((expected == Set(NONE) && resolved.isEmpty) || (resolved == expected), expected.toSeq.sorted.mkString(", "))
      }
    }

    def printInvocations() {
      val symbols: Set[Symbol] = reachableMethods ++ instantiatedClasses.map(_.typeSymbol)
      for {
        symbol <- symbols
        noInvocations = hasNoInvocationsAnnotation(symbol)
        if noInvocations || hasInvocationsAnnotation(symbol)
        expected = findAnnotationTargets(symbol, invocationsAnnotation, needProbe = false).toSet
        hasExpected = expected.nonEmpty
        if hasExpected || noInvocations
      } {
        assert(hasExpected != noInvocations, "@invocations should not be combined with @noInvocations for the same symbol")
        val methodOrConstructor: Symbol = if (symbol.isMethod) symbol else symbol.primaryConstructor
        val callSitesInMethodOrConstructor = if (callSitesInMethod.contains(methodOrConstructor))
          callSitesInMethod(methodOrConstructor)
        else Set()
        val resolved: Set[String] =
          callSitesInMethodOrConstructor.flatMap((cs: CallSite) =>
            callGraph(cs).map(cs.pos.line + ": " + findTargetAnnotation(_)))
        printCallGraph(resolved, isResolved = true)
        printCallGraph(expected, isResolved = false)
        assert(if (noInvocations) resolved.isEmpty else expected.subsetOf(resolved),
          (expected &~ resolved).toSeq.sorted.mkString(", "))
      }
    }
  }

  def printCallGraph(methodNames: Set[String], isResolved: Boolean) {
    val resolvedExpected = if (isResolved) "Resolved: " else "Expected: "
    println(resolvedExpected + methodNames.toSeq.sorted.mkString(", "))
  }

  def printCallGraph(out: java.io.PrintStream) {
    for {
      source <- reachableMethods
      sourceId = methodToId.getOrElse(source, 0)
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
      target <- callGraph(callSite)
      targetId = methodToId.getOrElse(target, 0)
    } out.println(sourceId + " " + targetId)
  }

  def printTextualCallGraph(out: java.io.PrintStream) {
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
      target <- callGraph(callSite)
    } out.println(probeMethod(source) + " ==> " + probeMethod(target))
  }

  private def formatPosition(pos: Position, method: Symbol) = {
    (if (pos.isDefined) relativize(pos.source.file) + " ::: " + pos.line
    else "unknown ::: 0") + " ::: " + probeMethod(method)
  }

  def printEclipseCallGraph(out: java.io.PrintStream) {
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
      target <- callGraph(callSite)
    } {
      out.println(
        formatPosition(callSite.pos, source) + " ==> " +
          formatPosition(target.pos, target))
    }
  }

  def printReachableMethods(out: java.io.PrintStream) {
    for (method <- reachableMethods)
      out.println(methodToId.getOrElse(method, 0) + " ::: " + formatPosition(method.pos, method))
  }

  /**
   * Print the mapping of all annotated methods to their source level signature.
   */
  def printMethods(out: java.io.PrintStream) {
    for (method <- methodToId.keys) {
      out.println(methodToId.getOrElse(method, 0) + " ===> " + probeMethod(method))
    }
  }

  /**
   * Print the mapping of the annotated methods to their source level effective owner.
   */
  def printMethodsOwners(out: java.io.PrintStream) {
    for (method <- methodToId.keys) {
      out.println(methodToId.getOrElse(method, 0) + " ===> " + effectiveOwnerName(method))
    }
  }

  /**
   * Return a probe call graph in GXL format.
   */
  def printProbeCallGraph(out: java.io.PrintStream) {
    val probeCallGraph = new CallGraph
    val entryPointsOut = new PrintStream("entrypoints.txt")
    val libraryOut = new PrintStream("library.txt")

    // Get the entry points (these include main method and callbacks)
    for {
      entry <- entryPoints ++ callbacks
    } {
      probeCallGraph.entryPoints.add(probeMethod(entry))
      entryPointsOut.println(methodToId.getOrElse(entry, 0) + " ===> " + probeMethod(entry))
    }
    entryPointsOut.close()

    // Get the edges
    for {
      source <- reachableMethods
      callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
      target <- callGraph(callSite)
      sourceFile = if (callSite.pos.isDefined) relativize(callSite.pos.source.file) else "unknown"
      line = if (callSite.pos.isDefined) callSite.pos.line.toString else "-1"
    } {
      val edge = new CallEdge(probeMethod(source), probeMethod(target), new CallSiteContext(sourceFile + " : line " + line))
      probeCallGraph.edges.add(edge)

      // Print out library methods to be added to the WALA call graph
      if (isLibrary(target)) libraryOut.println(edge.src + " ===> " + edge.dst)
    }
    libraryOut.close()

    // Write GXL file
    new GXLWriter().write(probeCallGraph, out)
  }

  private def formatPosition(pos: Position) = {
    if (pos.isDefined) relativize(pos.source.file) + " ::: " + pos.line
    else "unknown ::: 0"
  }

  /**
   * Get the relative path for an absolute source file path.
   */
  private def relativize(file: AbstractFile): String = {
    file.toString().replaceFirst(".+/build_src/[^/]+/", "")
  }

  /**
   * Get the relative path for an absolute source file path.
   */
  private def relativize(absolute: String): String = {
    absolute.replaceFirst(".+/build_src/[^/]+/", "")
  }

  val methodToId: collection.Map[Symbol, Int]
}

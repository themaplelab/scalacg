package ca.uwaterloo.scalacg.config

import scala.collection.immutable.{ Set => ImmutableSet }
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.util.Worklist
import probe.ProbeClass
import probe.ProbeMethod

/**
 * This is where all the global properties/finals shared across our code-base are found.
 */

trait Global {
  val global: scala.tools.nsc.Global
  import global._

  // Some extra string manipulation
  class StringHelper(str: String) {
    def replaceLast(regex: String, replacement: String) = {
      val arg1 = regex.reverse.replace("$", "\\$").replace(".", "\\.")
      val arg2 = replacement.reverse.replace("$", "\\$").replace(".", "\\.")
      str.reverse.replaceFirst(arg1, arg2).reverse
    }

    def replaceLastLiterally(literal: String, replacement: String) = {
      val arg1 = java.util.regex.Pattern.quote(literal)
      val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)
      replaceLast(literal, replacement)
    }
  }
  implicit def stringWrapper(string: String) = new StringHelper(string)

  // Symbol implicit helper to use with probe 
  class SymbolHelper(symbol: Symbol) {
    /**
     * Get the package name of a symbol by getting the ownerChain of the enclosing package, reverse, then drop the
     * root package, empty package, and any NoSymbol.
     */
    lazy val pkg = symbol.enclosingPackage.ownerChain.reverse.dropWhile(p => p.isRoot || p.isEmptyPackage).map(_.nameString).mkString(".")

    lazy val cls = symbol.enclClassChain.reverse.dropWhile(c => c.isPackageObjectOrClass).map(_.nameString).mkString(".")

    lazy val declaredInDefaultPackage = pkg == ""

    lazy val pkgDot = pkg + (if (declaredInDefaultPackage) "" else ".")

    lazy val nme = symbol.simpleName.decode

    lazy val sig = symbol.signatureString

    lazy val paramsSig = sig.substring(0, sig.indexOf(')') + 1).replace("(", "").replace(")", "")
  }
  implicit def symbolWrapper(symbol: Symbol) = new SymbolHelper(symbol)

  // This implicit helper to fix a bug in the toString for ProbeClass
  class ProbeClassHelper(cls: ProbeClass) {
    lazy val show = cls.pkg + "\t" + cls.name
    lazy val correctToString = if (cls.pkg == "") cls.name else cls.pkg + "." + cls.name
  }
  implicit def probeClassWrapper(cls: ProbeClass) = new ProbeClassHelper(cls)

  class ProbeMethodHelper(method: ProbeMethod) {
    lazy val show = method.cls.show + "\t" + method.name + "\t" + method.signature;
    lazy val correctToString = method.cls.correctToString + ": " + method.name + "(" + method.signature + ")";
  }
  implicit def probeMethodWrapper(method: ProbeMethod) = new ProbeMethodHelper(method)

}

trait CallGraphCollections extends CallSites with Global {
  import global._

  val callGraph: Map[CallSite, ImmutableSet[Symbol]]
  val entryPoints: Set[Symbol]
  val callBacks: Set[Symbol]
  val moduleConstructors: Set[Symbol]
}

trait CallGraphWorklists extends CallGraphCollections {
  import global._

  val instantiatedTypes: Worklist[Type]
  val callSites: Worklist[AbstractCallSite]
  val reachableMethods: Worklist[Symbol]
  val superCalled: Worklist[Symbol]
}

trait GlobalConstants {
  lazy final val annotationPackage = "ca.uwaterloo.scalacg.annotation"
  lazy final val annotationPackageSrc = "src/" + annotationPackage.replaceAllLiterally(".", "/")

  final val CallGraphPluginName = "callgraph"
  final val CallGraphPluginDescription = "builds a call graph"

  final val RunsAfterPhase = "uncurry" // The best place, so far, for our plugin to run after.

  final val AnnotationPhaseName = "methodannotation"
}
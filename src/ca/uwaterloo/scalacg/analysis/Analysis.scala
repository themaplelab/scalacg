package ca.uwaterloo.scalacg.analysis

import scala.collection.mutable.Set

import ca.uwaterloo.scalacg.config.CallGraphWorklists
import ca.uwaterloo.scalacg.plugin.PluginOptions
import ca.uwaterloo.scalacg.util.Lookup
import ca.uwaterloo.scalacg.util.MethodOps
import ca.uwaterloo.scalacg.util.SuperCalls
import ca.uwaterloo.scalacg.util.TreeTraversal
import ca.uwaterloo.scalacg.util.Worklist

/**
 * The various analysis options offered by our plugin.
 */
object Analysis extends Enumeration {
  type Analysis = Value

  val ra = Value("ra") // name-based analysis
  val tca_names = Value("tca-names") // name-based analysis limited to instantiated types
  val tca_bounds = Value("tca-bounds") // subtype-based analysis using abstract type bounds
  val tca_expand = Value("tca-expand") // subtype-based analysis using concretization of abstract types
  val tca_expand_this = Value("tca-expand-this") // tca-expand with special treatement of this

  final val Default = tca_expand_this // The analysis by default runs tca-expand-this
}

trait CallGraphAnalysis extends CallGraphWorklists
  with MethodOps
  with TreeTraversal
  with SuperCalls
  with Lookup {

  import global._

  // Initialize the worklists
  val instantiatedTypes = new Worklist[Type]
  val callSites = new Worklist[AbstractCallSite]
  val reachableMethods = new Worklist[Symbol]
  val superCalled = new Worklist[Symbol]

  // Initial params
  val entryPoints = Set[Symbol]()
  val callBacks = Set[Symbol]()
  val moduleConstructors = Set[Symbol]()
  val trees = Set[Tree]()

  // Plugin options
  val pluginOptions: PluginOptions

  var concretization: AbstractTypeConcretization = null

  def createConcretization: AbstractTypeConcretization =
    if (pluginOptions.analysis == Analysis.tca_bounds)
      new BoundsTypeConcretization
    else
      new TypeConcretization

  def initialize = {
    concretization = createConcretization

    // Get the ASTs from the current run
    trees ++= global.currentRun.units.map(_.body)

    // Karim: The ASTs have to be traversed at the very beginning. DO NOT try to traverse them on demand, doesn't work.
    trees.foreach(traverse(_, List()))

    // Main methods and the primary constructor of main modules are entry points to the call graph.
    entryPoints ++= mainMethods
    entryPoints ++= mainModulesPrimaryConstructors

    // Entry points are initially reachable, main modules are initially instantiated.
    reachableMethods ++= mainMethods
    instantiatedTypes ++=
      (if (pluginOptions.analysis == Analysis.ra)
        types
      else
        mainModules)

    // TODO
    //    for (method <- instantiatedTypesInMethod.keys) {
    //      if (instantiatedTypesInMethod(method).find(lineariztionStringOf(_) startsWith "org.kiama.output.Postfix").nonEmpty) {
    //        println(signature(method))
    //      }
    //    }

    // Gather some stats
    var abstractCallSitesCount = 0
    var abstractThisCallSitesCount = 0
    var abstractSuperCallSitesCount = 0

    var concreteCallSitesCount = 0
    var concreteThisCallSitesCount = 0
    var concreteSuperCallSitesCount = 0
  }

  var counter = 0
  def buildCallGraph = {
    while (reachableMethods.nonEmpty) {
      counter += 1
      // Debugging info
      println(s"Items in work list: ${reachableMethods.size}")

      // Process methods
      processMethods

      // Process types
      processTypes

      // Process super calls
      collectSuperCalled

      //      println("=" * 50)
      //      println(instantiatedTypes.reachableItems.map(_.typeSymbol.fullName contains "ConcreteType"))
      //      println("iteration " + counter)
      //      instantiatedTypes.reachableItems.foreach { tpe =>
      //        if (!(tpe.baseClasses.head.tpe =:= tpe)) {
      //          println(lineariztionStringOf(tpe))
      //          println(tpe.toString + " :: " + tpe.typeSymbol.isAbstractClass)
      //          println(tpe.members)
      //          println("\n")
      //        }
      //      }
      //      println("=" * 50)
      //      println("\n\n")

      // Process new call sites with all types, and use new types to process all call sites
      if (callSites.nonEmpty) {
        println(s"\tFound ${callSites.size} new call sites")
        processCallSites(callSites.newItems, instantiatedTypes.reachableItems)
      }
      if (instantiatedTypes.nonEmpty) {
        println(s"\tFound ${instantiatedTypes.size} new instantiated types")
        processCallSites(callSites.reachableItems, instantiatedTypes.newItems)
      }

      // Clear call sites and instantiated types to prepare for the next iteration.
      callSites.clear
      instantiatedTypes.clear
    }
  }

  /**
   * Process newly instantiated types.
   */
  private def processTypes = {
    for (tpe <- instantiatedTypes.newItems) {
      // Add constructors
      val constrs = constructorsOf(tpe)
      if (tpe.typeSymbol.isModuleOrModuleClass) moduleConstructors ++= constrs
      reachableMethods ++= constrs

      // Add callbacks
      val cbs = callBacksOf(tpe)
      callBacks ++= cbs
      reachableMethods ++= cbs

      // Add type concretizations 
      concretization.addTypeConcretization(tpe)
    }
  }

  /**
   * Process newly reachable methods.
   */
  private def processMethods = {
    for (method <- reachableMethods.newItems) {
      // Find new call sites
      callSites ++= callSitesInMethod(method)

      // Find new instantiated types
      instantiatedTypes ++= instantiatedTypesInMethod(method)

      // Find TypeApplys that instantiate method type parameters
      for (ta <- typeAppliesInMethod(method)) {
        concretization.addMethodInstantiation(ta.fun.symbol, ta.args.map(_.tpe))
      }
    }

    reachableMethods.clear
  }

  /**
   * Collect all methods that are called via "super".
   */
  private def collectSuperCalled = {
    for (callSite <- callSites.newItems) {
      if (callSite.isSuperCall) {
        superCalled += callSite.staticTarget
      }
    }
  }

  /**
   * Process some call sites given some types.
   */
  private def processCallSites(callSites: Set[AbstractCallSite], types: Set[Type]) = {
    for (callSite <- callSites) {
      val targets = Set[Symbol]()

      if (callSite.isConstructorCall) {
        targets += processConstructorCall(callSite)
      } else if (callSite.isFunctionCall) {
        targets += processFunctionCall(callSite)
      } else if (pluginOptions.doSuperCalls && callSite.isSuperCall) {
        targets ++= processSuperCall(callSite, types)
      } else if (pluginOptions.doThis && callSite.hasThisReceiver) {
        targets ++= processThisCall(callSite, types)
      } else {
        targets ++= processLookup(callSite, types)
      }

      // Add targets to worklist
      reachableMethods ++= targets
    }
  }

  /**
   * If the target method is a constructor, no need to do the lookup.
   */
  private def processConstructorCall(callSite: AbstractCallSite) = {
    useStaticTarget(callSite)
  }

  /**
   * If the receiver of the call is a function, no need to do the lookup.
   */
  private def processFunctionCall(callSite: AbstractCallSite) = {
    useStaticTarget(callSite)
  }

  /**
   * Use the static target as the only target for this call site
   * for those cases where no lookup is needed.
   */
  private def useStaticTarget(callSite: AbstractCallSite) = {
    addTargets(callSite, Set(callSite.staticTarget))
    callSite.staticTarget
  }

  /**
   * If it's a super call, call lookupSuper with types that contain the enclosing class of the receiver
   * in their linearization (i.e., tpe.baseClasses).
   */
  private def processSuperCall(callSite: AbstractCallSite, types: Set[Type]) = {
    val targets = lookupSuper(callSite, types)
    addTargets(callSite, targets)
    targets
  }

  /**
   * IF it's a "this" call, then use
   */
  private def processThisCall(callSite: AbstractCallSite, types: Set[Type]) = {
    val allTargets = Set[Symbol]()

    abstractToCallSites(callSite).foreach { cs =>
      //      println("resolving call site: " + cs)
      val lookupTypes = filterForThis(cs, types)
      //      (types diff lookupTypes).foreach { tpe => println("\t excluding type " + tpe + " because it contains a definition for " + signature(cs.thisEnclMethod)) }
      val targets = lookup_<:<(cs, lookupTypes)

      addTargets(cs, targets)
      allTargets ++= targets
    }

    allTargets
  }

  /**
   * Process a normal lookup for the given call site using the given types.
   */
  private def processLookup(callSite: AbstractCallSite, types: Set[Type]) = {
    val targets = lookup_<:<(callSite, types)
    addTargets(callSite, targets)
    targets
  }

  /**
   * Add edges from each real call site abstracted by callSite to the given set of targets.
   */
  private def addTargets(callSite: AbstractCallSite, targets: Set[Symbol]) = {
    abstractToCallSites(callSite).foreach { cs =>
      callGraph(cs) ++= targets

      // TODO
      if (callSite.targetName.toString == "category") {
        if (cs.position.line == 383 && cs.position.source.file.toString.contains("OntonotesChainNer.scala")) {
          println("HOBBA: " + cs.receiver + " :: " + signature(cs.enclMethod) + " :: " + (targets map signature))
        }
      }
    }
  }

  /**
   * Add edges from a given call site to some given targets.
   */
  private def addTargets(callSite: CallSite, targets: Set[Symbol]) = {
    callGraph(callSite) ++= targets
  }

  /**
   * Filter the types that will be used later to lookup for methods if the receiver of the call site is "this".
   * If tpe.members contains callSite.thisEnclMethod, that means that tpe doesn't override callSite.thisEnclMethod,
   * and it is possible to call one of tpe's methods at this call site.
   *
   * TODO: filter needs OPT?
   */
  def filterForThis(callSite: CallSite, types: Set[Type]) = {
    if (callSite.enclMethod.isConstructor || callSite.thisEnclMethod == NoSymbol || superCalled.reachableItems.contains(callSite.thisEnclMethod)) types
    //    else types.filter { tpe => tpe.members.toSet contains callSite.thisEnclMethod }
    else types intersect thisEnclMethodToTypes(callSite.thisEnclMethod)
  }
}
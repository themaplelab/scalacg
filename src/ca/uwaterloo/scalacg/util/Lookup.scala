package ca.uwaterloo.scalacg.util

import scala.collection.mutable.Set
import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.analysis.Analysis
import ca.uwaterloo.scalacg.config.Global
import ca.uwaterloo.scalacg.plugin.PluginOptions

/**
 * All method resolution related operations go in here.
 */
trait Lookup extends Probe {
  self: CallSites with Global with TypeOps =>

  import global._

  // Plugin options
  val pluginOptions: PluginOptions

  /**
   * Look for targets at the given call site in the given type.
   */
  def lookup(callSite: AbstractCallSite, tpe: Type) = {
    val targets = Set[Symbol]()

    for {
      expanded <- concretization.expand(instantiateTypeParams(tpe, callSite.receiver.widen))
      target = member(expanded, tpe, callSite.targetName)
      if target != NoSymbol && !target.isDeferred // a concrete member found.
    } {
      targets ++= getTargets(target, callSite.staticTarget)
    }

    targets
  }

  /**
   * Find a member method.
   */
  def member(expanded: Type, tpe: Type, targetName: Name) = {
    val (expandedAsSeenFromTpe, tpeAsSeenFromExpanded) = asSeenFrom(expanded, tpe)
    tpeAsSeenFromExpanded.member(targetName)
  }

  /**
   * Look for targets at the given call site using the given types. This is the normal method resolution that obeys
   * sub-typing relationships.
   */
  def lookup_<:<(callSite: AbstractCallSite, types: collection.Set[Type]): Set[Symbol] = {
    val targets = Set[Symbol]()

    for (tpe <- types) {
      pluginOptions.analysis match {
        case Analysis.Ra | Analysis.Tcra =>
          targets ++= lookupByName(callSite, tpe)
        case _ =>
          targets ++= lookup_<:<(callSite, tpe)
      }
    }

    // Handle calls to the library.
    if (isLibrary(callSite.staticTarget)) {
      targets += callSite.staticTarget
    }

    targets
  }

  /**
   * Look for targets at the given call site in the given type, conforming to the sub-typing rules.
   */
  def lookup_<:<(callSite: AbstractCallSite, tpe: Type) = {
    val targets = Set[Symbol]()

    for {
      expanded <- concretization.expand(instantiateTypeParams(tpe, callSite.receiver.widen))
      target = member_<:<(expanded, tpe, callSite.targetName)
      if target != NoSymbol && !target.isDeferred // a concrete member found.
    } {
      targets ++= getTargets(target, callSite.staticTarget)

      //      if (callSite.targetName containsName "toText") {
      //      if (targets.exists(signature(_) == "<tests.tca.AbstractClassObject.Type: toText()>")) {
      //        println(callSite)
      //        println(tpe + " :: " + expanded)
      //        println(targets map signature)
      //        println(tpe.getClass)
      //        println(tpe.members)
      //        throw new IllegalStateException("adding edge to Type.toText")
      //      }

      //      if (callSite.staticTarget.nameString == "toString") { // TODO
      //        println("Found targets: " + callSite.receiver + " :: " + tpe + " :: " + expanded + " :: " + (targets map signature))
      //      }
    }

    targets
  }

  /**
   * Look for targets at the given call site in the given type, using only the target name.
   */
  def lookupByName(callSite: AbstractCallSite, tpe: Type) = {
    val target = tpe.member(callSite.targetName)
    if(target == NoSymbol) Set()
    else if(target.isDeferred) Set()
    else if(target.isOverloaded) target.alternatives
    else Set(target)
  }

  /**
   * Find a member method in a super-type, as seen from the sub-type.
   */
  def member_<:<(expanded: Type, tpe: Type, targetName: Name) = {
    val (expandedAsSeenFromTpe, tpeAsSeenFromExpanded) = asSeenFrom(expanded, tpe)
    //    if (expanded.toString == "tests.tca.AbstractClassObject.Type"
    //      && tpe.toString == "tests.tca.AbstractClassObject.TString.type" && targetName.containsName("toText")) {
    //      println(tpe + " :: " + tpeAsSeenFromExpanded)
    //      println(expanded + " :: " + expandedAsSeenFromTpe)
    //      println(targetName)
    //      println(signature(tpeAsSeenFromExpanded.member(targetName)))
    //      throw new IllegalStateException("adding edge to Type.toText")
    //    }
    if (tpeAsSeenFromExpanded <:< expandedAsSeenFromTpe) tpeAsSeenFromExpanded.member(targetName)
    else NoSymbol
  }

  /**
   * Get all possible targets (including overloaded ones).
   */
  def getTargets(target: Symbol, staticTarget: Symbol) = {
    val targets = Set[Symbol]()
    //    if (target.name containsName "toText") {
    //      println(target + " :: " + target.isOverloaded)
    //      throw new IllegalStateException("adding edge to Type.toText")
    //    }
    target match {
      case _ =>
        // Disambiguate overloaded methods based on the types of the args
        if (target.isOverloaded) {
          targets ++= target.alternatives filter (_.tpe matches staticTarget.tpe)
        } else {
          targets += target
        }
    }
    targets
  }

  def lookupThis(callSite: CallSite, types: Set[Type]) = {

  }
}
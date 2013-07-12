package callgraph

import scala.collection.mutable
import scala.tools.nsc
import scala.collection.immutable.Set
import scala.Predef._

trait RA extends CGUtils {
  val global: nsc.Global

  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  var superMethodNames = Set[TermName]()

  val classToMembers = mutable.Map[Type, Set[Symbol]]()

  var instantiatedClasses = Set[Type]()

  // in Scala, code can appear in classes as well as in methods, so reachableCode generalizes reachable methods
  var reachableCode = Set[Symbol]()

  var callbacks = Set[Symbol]()

  // newly reachable methods to be processed
  val methodWorklist = mutable.Queue[Symbol]()

  var cache = Map[Name, Set[Symbol]]()

  def addMethod(method: Symbol) = if (!reachableCode(method)) methodWorklist += method

  def nameLookup(staticTarget: Symbol, classes: Set[Type],
    lookForSuperClasses: Boolean = false, getSuperName: (String => String) = (n: String) => n): Set[Symbol] = {

    // Don't lookup a call to a constructor
    if (staticTarget.isConstructor) {
      return Set(staticTarget)
    }

    // Do we have the result cached?
    if (!(cache contains staticTarget.name)) {
      // Lookup the targets by name
      var targets = classes.flatMap(_.members.filter((m: Symbol) =>
        m.name == (if (lookForSuperClasses) staticTarget.name.newName(getSuperName(staticTarget.name.toString)) else staticTarget.name)
          && m.isMethod))

      // Add the static target if it's in the library
      if (isLibrary(staticTarget)) {
        targets += staticTarget
      }

      // Add to cache
      cache += (staticTarget.name -> targets)
    }

    return cache(staticTarget.name)
  }

  def buildCallGraph() {

    classes = appClasses

    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    // add all constructors
    classes.map(_.typeSymbol).foreach(addMethod)
    for {
      cls <- classes
      constr <- cls.members
      if constr.isConstructor
    } {
      addMethod(constr)
    }

    // Library call backs are also reachable
    for {
      cls <- classes
      member <- cls.decls // loop over the declared members, "members" returns defined AND inherited members
      if isApplication(member) && isOverridingLibraryMethod(member)
    } {
      callbacks += member
    }
    callbacks.foreach(addMethod)

    //    var debug = false

    while (methodWorklist.nonEmpty) {
      // Debugging info
      println("Items in work list: " + methodWorklist.size)

      //      debug = methodWorklist.size <= 2

      // process new methods
      for (method <- methodWorklist.dequeueAll(_ => true)) {
        reachableCode += method
      }

      // process all call sites in reachable code
      for {
        callSite <- callSites
        if reachableCode contains callSite.enclMethod
      } {
        //        if(debug) println(callSite.enclMethod + " ===> " + callSite.staticTarget)
        val csStaticTarget = callSite.staticTarget
        val superTargets = if (isSuperCall(callSite))
          nameLookup(csStaticTarget, classes, lookForSuperClasses = true, getSuperName = superName)
        else Set()
        val targets = nameLookup(csStaticTarget, classes) ++ superTargets
        callGraph += (callSite -> targets)
        targets.foreach(addMethod)
      }
    }
    
    // Debugging info
      println("Work list is empty now.")
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}
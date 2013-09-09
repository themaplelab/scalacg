package callgraph.analysis.util

import reflect.internal.Flags._
import collection.immutable.Set
import callgraph.analysis.TreeTraversal
import callgraph.analysis.output.Probe

trait SuperCalls extends Probe {

  this: TreeTraversal with Lookup =>

  import global._

  def superLookup(callSite: CallSite, consideredClasses: Set[Type]): collection.Set[Symbol] = {
    lookup(callSite, consideredClasses, lookForSuperClasses = true)
  }

  def superName = ((name: String) => {
    val super$Prefix = "super$"
    if (name.startsWith(super$Prefix))
      name.substring(super$Prefix.length)
    else name
  })

  def isSuperCall(callSite: CallSite): Boolean =
    superReceiverOption(callSite.receiver).isDefined || callSite.staticTarget.hasFlag(SUPERACCESSOR)

  def superReceiverOption(receiver: Tree): Option[TermName] = {
    receiver match {
      case Super(_, name) => Some(name)
      case _ => None
    }
  }

  /* returns pair of set of super targets and boolean indicating whether the super call is of form super[T] */
  def getSuperTargets(callSite: CallSite,
                      classes: Set[Type],
                      typeDependent: Boolean = false):
      collection.Set[Symbol] = {

    val csStaticTarget = callSite.staticTarget

    /* RA super targets resolution*/
    if (!typeDependent) {
      if (isSuperCall(callSite))
        return lookup(callSite, classes, lookForSuperClasses = true)
      return Set()
    }

    /* TCA-style super targets resolution */
    val receiver = callSite.receiver
    val superReceiverName = superReceiverOption(receiver)
    val csEnclClass = csStaticTarget.enclClass
    if (superReceiverName.isDefined) {
      superReceiverName match {
        case Some(name) =>
          if (name.isEmpty)
            return Set(csStaticTarget)
          else {
            val bcs = csEnclClass.baseClasses
            val superClass = bcs.find(_.nameString == name.toString) // todo: filter instead of find??
            if (superClass.isDefined) {
              return lookup(callSite, Set(superClass.get.tpe))
            }
          }
        case _ =>
      }
    }

    if (isSuperCall(callSite)) {
      val classLinearizations: Set[List[Symbol]] = classes.map(_.baseClasses)
      return classLinearizations.collect {
        case classLin if classLin contains csEnclClass =>
          val startFrom = classLin indexOf csEnclClass
          val dropped: List[Symbol] = classLin.drop(startFrom).tail
          // find the first class in the list of linearized base classes, starting from index 'startFrom',
          // that contains a method with same signature as csStaticTarget
          dropped.collectFirst {  // todo: returns none?
            case cl if superLookup(callSite, Set(cl.tpe)).nonEmpty => superLookup(callSite, Set(cl.tpe))
          }.getOrElse(Set())
      }.flatten
    } else Set()
  }

  def isReachableSuperMethodName(method: Symbol, superMethodNames: Set[TermName], reachableCode: Set[Symbol]): Boolean =
    superMethodNames.contains(method.name) && reachableCode.contains(method)

  def getNewSuperCalls(reachableCode: Set[Symbol]): Set[Symbol] = {
    var superCalls = Set[Symbol]()
    for {
      callSite <- callSites
      if isSuperCall(callSite)
      if reachableCode contains callSite.enclMethod
    } {
      superCalls += callSite.staticTarget
    }
    superCalls
  }
  
  def findSuperCalls(callSites: collection.Set[CallSite]) = {
    var superCalls = Set[Symbol]()
    for {
      callSite <- callSites
      if isSuperCall(callSite)
    } {
      superCalls += callSite.staticTarget
    }
    superCalls
  }
}

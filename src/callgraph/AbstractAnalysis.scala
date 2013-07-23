package callgraph


import scala.collection.mutable

import ca.uwaterloo.scalacg.util.CGAnnotations
import ca.uwaterloo.scalacg.util.Probe
import util.{Lookup, LibraryCalls}

trait AbstractAnalysis extends TreeTraversal with Lookup with LibraryCalls with CGAnnotations with Probe {

  import global._

  var instantiatedClasses = Set[Type]()
  var reachableCode = Set[Symbol]()
  var callbacks = Set[Symbol]()

  def appClasses: Set[Type]

  // application classes fed to the compiler (i.e., not including Scala/Java libraries)
  def callGraph: CallSite => Set[Symbol]

  var classes = Set[Type]()

  def getClasses: Set[Type]

  def initialize() {
    // find the set of instantiated classes in the whole program
    classes = getClasses

    // find call sites
    trees.foreach {
      tree =>
        findCallSites(tree, List())
    }
  }

  // newly reachable methods to be processed
  val methodWorklist = mutable.Queue[Symbol]()

  def addMethod(method: Symbol) = if (!reachableCode(method)) methodWorklist += method

  private def transitiveClosure[T](initial: Set[T], transition: T => Set[T]): Set[T] = {
    val seen = mutable.Set[T]() ++ initial
    val queue = mutable.Queue[T]() ++ initial
    while (!queue.isEmpty) {
      val item = queue.dequeue()
      val image = transition(item) -- seen
      queue ++= image
      seen ++= image
    }
    Set() ++ seen
  }

  lazy val entryPoints = mainMethods

  // return all main methods that are inherited into some object
  private def mainMethods = {
    val mainName = stringToTermName("main")

    val mainMethods = classes.filter(_.typeSymbol.isModuleOrModuleClass). // filter classes that are objects
      collect {
        case cs: ModuleTypeRef => cs.member(mainName)
      }. // collect main methods
      filter(m => m.isMethod && // consider only methods, not fields or other members
        !m.isDeferred && // filter out abstract methods
        m.typeSignature.toString().equals("(args: Array[String])Unit")) // filter out methods accidentally named "main"

    // global.definitions.StringArray

    Set() ++ mainMethods
  }

  lazy val reachableMethods = transitiveClosure(entryPoints ++ callbacks, {
    source: Symbol =>
      for {
        callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
        target <- callGraph(callSite)
      } yield target: Symbol
  })
}

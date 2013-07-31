package callgraph.analysis

import output.CGAnnotations
import scala.collection.mutable

import collection.immutable.Set
import util.{Global, LibraryCalls, Lookup}

trait AbstractAnalysis extends TreeTraversal with Lookup with LibraryCalls with CGAnnotations with Global {

  import global._

  var types = Set[Type]()
  var reachableCode = Set[Symbol]()
  var callbacks = Set[Symbol]()
  var callGraph = Map[CallSite, Set[Symbol]]()

  def getTypes: Set[Type] = {
    trees.flatMap {
      tree =>
        tree.collect {
          case cd: ClassDef => cd.symbol.tpe
          case nw: New => nw.tpt.tpe // to get the library types used in the application
        }
    }.toSet
  }

  def initialize() {
    // find the set of instantiated classes in the whole program
    types = getTypes

    // find call sites
    trees.foreach {
      tree =>
        findCallSites(tree, List())
    }
  }
  
  def buildCallGraph()

  private def transitiveClosure[T](initial: Set[T], transition: T => Set[T]): Set[T] = {
    val seen = mutable.Set[T]() ++ initial
    val queue = mutable.Queue[T]() ++ initial
    while (!queue.isEmpty) {
      val item = queue.dequeue()
      val image = transition(item) -- seen
      queue ++= image
      seen ++= image
    }
    seen.toSet[T]
  }

  lazy val entryPoints = mainMethods

  // return all main methods that are inherited into some object
  private def mainMethods = {
    val mainName = stringToTermName("main")

    val mainMethods = types.filter(_.typeSymbol.isModuleOrModuleClass). // filter classes that are objects
      collect {
        case cs: ModuleTypeRef => cs.member(mainName)
      }. // collect main methods
      filter(m => m.isMethod && // consider only methods, not fields or other members
        !m.isDeferred && // filter out abstract methods
        m.typeSignature.toString().equals("(args: Array[String])Unit")) // filter out methods accidentally named "main"

    // global.definitions.StringArray

    mainMethods
  }

  lazy val reachableMethods = transitiveClosure(entryPoints ++ callbacks, {
    source: Symbol =>
      for {
        callSite <- callSitesInMethod.getOrElse(source, Set()).filter(reachableCode contains _.enclMethod)
        target <- callGraph(callSite)
      } yield target: Symbol
  })
}

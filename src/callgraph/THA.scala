package callgraph

import scala.collection.mutable
import scala.tools.nsc
import scala.reflect.internal.Flags._
import scala.collection.immutable.Set

trait THA extends CGUtils {
  val global: nsc.Global
  import global._

  var callGraph = Map[CallSite, Set[Symbol]]()

  var superMethodNames = Set[TermName]()

  override def initialize() {
    super.initialize()
  }

  val classToMembers = mutable.Map[Type, Set[Symbol]]()

  var instantiatedClasses = Set[Type]()

  // in Scala, code can appear in classes as well as in methods, so reachableCode generalizes reachable methods
  var reachableCode = Set[Symbol]()

  var callbacks = Set[Symbol]()

  // newly reachable methods to be processed
  val methodWorklist = mutable.Queue[Symbol]()

  def addMethod(method: Symbol) = {
    if (!reachableCode(method)) methodWorklist += method
  }

  // the set of classes instantiated in a given method
  lazy val classesInMethod = {
    val ret = mutable.Map[Symbol, Set[Type]]().withDefaultValue(Set())
    def traverse(tree: Tree, owner: Symbol) {
      tree match {
        case _: DefDef =>
          tree.children.foreach(traverse(_, tree.symbol))
        case _: ClassDef => // If the tree is a class definition, then "owner" should be the primary constructor (see GetterMethod1) 
          tree.children.foreach(traverse(_, tree.symbol.primaryConstructor))
        case New(tpt) =>
          ret(owner) += tpt.tpe.dealias // some types are aliased, see CaseClass3
        case _ =>
          tree.children.foreach(traverse(_, owner))
      }
    }
    trees.foreach(traverse(_, NoSymbol))
    ret
  }

  private def superReceiverOption(receiver: Tree): Option[TermName] = {
    receiver match {
      case Super(_, name) => Some(name)
      case _ => None
    }
  }

  private def isSuperCall(callSite: CallSite): Boolean = {
    superReceiverOption(callSite.receiver).isDefined || callSite.staticTarget.hasFlag(SUPERACCESSOR)
  }

  def buildCallGraph() {
    /* Given the ancestors of a This in the AST, determines the method that has that
     * particular This as its implicit this parameter.
     * 
     * It is possible that there is no such method. For example:
     * class A {
     *   val a = 5
     *   val b = this.a + 6
     * }
     * In such cases, returns NoSymbol
     * 
     * Karim: For these cases, such a method is the primary constructor of the class.
     */
    def containingMethod(ancestors: List[Tree], thisType: Symbol): Symbol = {
      (for {
        firstContainer <- ancestors.find { node =>
          node.isInstanceOf[DefDef] || node.isInstanceOf[ClassDef]
        }
        instanceMethod <- firstContainer.symbol.ownersIterator.find { sym =>
          sym.isMethod && sym.owner == thisType
        }
      } yield instanceMethod).getOrElse(NoSymbol)
    }

    // todo: qualified super calls (C.super.m)
    // todo: super[T].m
    def getSuperSymbols(callSite: CallSite): Set[Symbol] = {

      def overrideChain(s: Symbol) = (
        if (s eq NoSymbol) Nil
        else if (s.isOverridingSymbol) s :: s.allOverriddenSymbols
        else s :: Nil)

      val csStaticTarget = callSite.staticTarget
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
              val names = bcs.map(_.name)
              val superClass = bcs.find(_.nameString == name.toString) // todo: filter instead of find??
              if (superClass.isDefined) {
                return lookup(receiver.tpe, csStaticTarget, Set(superClass.get.tpe))
              }
            }
          case _ =>
        }
      }

      if (isSuperCall(callSite)) {
        val classLinearizations: Set[List[Symbol]] = instantiatedClasses.map(_.baseClasses)
        val superCalls: Set[Symbol] = classLinearizations.collect {
          case classLin if classLin contains csEnclClass =>
            val startFrom = classLin indexOf csEnclClass
            val dropped: List[Symbol] = classLin.drop(startFrom).tail
            // find the first class in the list of linearized base classes, starting from index 'startFrom',
            // that contains a method with same signature as csStaticTarget
            dropped.collectFirst {
              case cl if superLookup(receiver.tpe, csStaticTarget, Set(cl.tpe)).nonEmpty => superLookup(receiver.tpe, csStaticTarget, Set(cl.tpe))
            }.getOrElse(Set())
        }.flatten
        return superCalls.toSet
      }
      Set()
    }

    // all objects are considered to be allocated
    // Karim: Here isModuleOrModuleClass should be used instead of just isModule, or isModuleClass. I have no idea
    // why this works this way, but whenever I use either of them alone something crashes.
    instantiatedClasses ++= classes.filter(_.typeSymbol.isModuleOrModuleClass)

    // start off the worklist with the entry points
    methodWorklist ++= entryPoints

    while (methodWorklist.nonEmpty) {
      // process new methods
      for (method <- methodWorklist.dequeueAll(_ => true)) {
        reachableCode += method
        instantiatedClasses ++= classesInMethod(method)
      }
      // find call sites that use super (e.g., super.foo())
      // Now this has been moved inside the worklist (see ThisType2)
      for {
        callSite <- callSites
        if reachableCode contains callSite.enclMethod
        if (isSuperCall(callSite))
      } {
        superMethodNames += callSite.staticTarget.name
      }

      def mySuperSymbol(s: Symbol, base: Symbol, owner: Symbol): Symbol = {
        var bcs = base.info.baseClasses.dropWhile(owner != _).tail
        var sym: Symbol = NoSymbol
        while (!bcs.isEmpty && sym == NoSymbol) {
          if (!bcs.head.isImplClass)
            sym = s.matchingSymbol(bcs.head, base.thisType).suchThat(!_.isDeferred)
          bcs = bcs.tail
        }
        sym
      }

      for {
        cs <- callSites
        st = cs.staticTarget
        if st.nameString.contains("k")
        ic <- instantiatedClasses
        if ic.baseClasses.contains(st.enclClass)
      } {
        println("st = " + st)
        println("instCl = " + ic)
        println("bcs = " + ic.baseClasses)
        val s = mySuperSymbol(st, ic.typeSymbol, st.enclClass)
        val p = st.getClass()
        println("supS = " + s)
      }

      // process all call sites in reachable code
      for {
        callSite <- callSites
        if reachableCode contains callSite.enclMethod
      } {
        var targets = Set[Symbol]()

        val receiver = callSite.receiver
        val csStaticTarget: MethodSymbol = callSite.staticTarget
        if (receiver == null) {
          targets = Set(csStaticTarget)
        } else {
          val thisSymbol =
            if (receiver.isInstanceOf[This])
              receiver.symbol
            else if (receiver.tpe.isInstanceOf[ThisType])
              receiver.tpe.asInstanceOf[ThisType].sym
            else NoSymbol
          val filteredClasses: Set[Type] =
            thisSymbol match {
              case NoSymbol => instantiatedClasses
              case symbol =>
                val method = containingMethod(callSite.ancestors, symbol)
                // TODO: need to change this because super might occur in unreachable code (ThisType2)
                if (method == NoSymbol || superMethodNames.contains(method.name))
                  instantiatedClasses
                else
                  instantiatedClasses.filter { tpe =>
                    val members = classToMembers.getOrElseUpdate(tpe, tpe.members.sorted.toSet)
                    members.contains(method)
                  }
            }
          val superSymbols = getSuperSymbols(callSite)
          targets = lookup(receiver.tpe, csStaticTarget, filteredClasses) ++ superSymbols // todo: for super[T] lookup here not necessary
        }

        callGraph += (callSite -> targets)
        targets.foreach(addMethod)
      }

      // add all constructors
      // TODO Karim: I don't understand how this adds class definition to reachable code? how is this later processed?
      instantiatedClasses.map(_.typeSymbol).foreach(addMethod)
      for {
        cls <- instantiatedClasses
        constr <- cls.members
        if constr.isConstructor
      } {
        addMethod(constr)
      }

      // Library call backs are also reachable
      for {
        cls <- instantiatedClasses
        member <- cls.decls // loop over the declared members, "members" returns defined AND inherited members
        if libraryOverriddenMethods(member).nonEmpty
      } {
        callbacks += member
      }
      callbacks.foreach(addMethod)

      // Type concretization now should happen inside the worklist too, and only for the instantiated classes
      // This should improve the precision of our analysis 
      addTypeConcretizations(instantiatedClasses)
    }
  }

  val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }
}

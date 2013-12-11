package ca.uwaterloo.scalacg.util

import scala.collection.immutable.{Set => ImmutableSet}
import scala.collection.mutable.Map
import scala.collection.mutable.Set

import ca.uwaterloo.scalacg.analysis.CallSites
import ca.uwaterloo.scalacg.config.Global

/**
 * Holds any tree-related information
 */

trait Trees extends Global {
  import global._

  val trees: Set[Tree]
}

trait TraversalCollections extends Global with CallSites with TypesCollections with MethodsCollections {
  import global._

  val abstractCallSites: Set[AbstractCallSite]
  val abstractToCallSites: Map[AbstractCallSite, ImmutableSet[CallSite]]
  val callSitesInMethod: Map[Symbol, ImmutableSet[AbstractCallSite]]
  val instantiatedTypesInMethod: Map[Symbol, ImmutableSet[Type]]
  val typeAppliesInMethod: Map[Symbol, ImmutableSet[TypeApply]]
}

trait TreeTraversal extends Trees with TraversalCollections with TypeOps {
  import global._
  // Benchmark characteristics
  var classesCount = 0
  var anonfunCount = 0
  var objectsCount = 0
  var traitsCount = 0
  var traitCompsCount = 0
  var closuresCount = 0
  var methodsCount = 0
  var classesAtmCount = 0
  var classesAtpCount = 0

  // Call sites characteristics
  var callSitesTotalCount = 0
  var callSitesThisCount = 0
  var callSitesSuperCount = 0
  var callSitesAbstractTypesCount = 0

  var overridingMethodsCount = 0

  // Traversal
  val abstractCallSites = Set[AbstractCallSite]()
  val abstractToCallSites = Map[AbstractCallSite, ImmutableSet[CallSite]]().withDefaultValue(ImmutableSet.empty[CallSite])
  val callSitesInMethod = Map[Symbol, ImmutableSet[AbstractCallSite]]().withDefaultValue(ImmutableSet.empty[AbstractCallSite])
  val instantiatedTypesInMethod = Map[Symbol, ImmutableSet[Type]]().withDefaultValue(ImmutableSet.empty[Type])
  val typeAppliesInMethod = Map[Symbol, ImmutableSet[TypeApply]]().withDefaultValue(ImmutableSet.empty)

  // Types
  val applicationTypes = Set[Type]()
  val types = Set[Type]()
  val mainModules = Set[Type]()
  val instantiated = Set[Type]() // a local set so that we do not process instantiated types more than once
  val thisEnclMethodToTypes = Map[Symbol, ImmutableSet[Type]]().withDefaultValue(ImmutableSet.empty[Type])
  val packageNames = Set[String]()

  // Methods
  val mainMethods = Set[Symbol]()
  val mainModulesPrimaryConstructors = Set[Symbol]()

  // Find receiver annotations
  private val annotationFilter: PartialFunction[Tree, String] = {
    case Literal(Constant(string: String)) => string
    // TODO: replace _ with a more specific check for the cha case class
    case Apply(_, List(Literal(Constant(string: String)))) => string
  }

  // Traverse the ASTs and collect information
  def traverse(tree: Tree, ancestors: List[Tree]): Unit = {
    tree match {
      case cls: ClassDef => findCallSitesInClassDef(cls)
      case module: ModuleDef => findType(module)
      case nw: New => findInstantiatedTypes(nw)
      case apply: Apply => findCallSitesInApply(apply)
      case _: Select | _: Ident => findCallSitesSelectOrIdent // TODO: This causes duplicate call sites (see foreach in Reachable1b)
      case dd: DefDef => updateMethodsCount(dd)
      case vd: ValDef => updateClosuresCount(vd)
      case ta: TypeApply =>
        val method = enclMethod
        typeAppliesInMethod += (method -> (typeAppliesInMethod(method) + ta))
        processChildren
      case _ => processChildren
    }

    def updateMethodsCount(dd: DefDef) = {
      methodsCount += 1
      val symbol = dd.symbol

      // count only concrete methods, because those are the ones that can have "this" call sites in them
      if (!symbol.isConstructor &&
        symbol.isMethod &&
        !symbol.isDeferred &&
        symbol.isOverridingSymbol) overridingMethodsCount += 1
      processChildren
    }

    def updateClosuresCount(vd: ValDef) = {
      // count only uninitialized closures, initialized ones are counted for by anonfun
      if (definitions.isFunctionType(vd.symbol.tpe) && vd.rhs.isEmpty) closuresCount += 1
      processChildren
    }

    /**
     * Adds an instantiated type to its enclMethod.
     */
    def addInstantiatedType(tpe: Type) = {
      //      if (tpe.typeSymbol.isAbstractClass || tpe.typeSymbol.isAbstractType) println("abstract : " + tpe)
      //      println(tpe + " :: " + (tpe.typeSymbol.name containsName "ConcreteType"))
      //      if (tpe.typeSymbol.name containsName "TString") {
      //        println("adding a new instance of TString from tree: " + tpe.getClass)
      //        println(tpe.typeSymbol.isModuleOrModuleClass)
      //        println(tpe.typeSymbol + " :: " + tpe.typeSymbol.tpe.getClass)
      //      }
      val typeToAdd = if (tpe.typeSymbol.isModuleOrModuleClass) tpe.typeSymbol.tpe else tpe
      instantiatedTypesInMethod(enclMethod) += typeToAdd
      if (!instantiated(tpe)) tpe.members filter (_.isMethod) foreach { method => thisEnclMethodToTypes(method) += tpe }
    }

    // Add a type encountered traversing the tree (this could be an instantiated or a defined type).
    def addType(tree: Tree) = tree match {
      case _: ClassDef | _: ModuleDef => {
        val cls = tree.symbol // NSC marks this as OPT, so just call it once
        val tpe = cls.tpe

        // Do not process a class/module definition multiple times
        if (!applicationTypes(tpe)) {
          applicationTypes += tpe
          types += tpe

          // Update some stats
          if (cls.isTrait) traitsCount += 1
          else if (cls.isModuleOrModuleClass) objectsCount += 1
          else if (cls.isAnonymousFunction) anonfunCount += 1
          else if (cls.isClass) classesCount += 1

          if (cls.mixinClasses.nonEmpty) traitCompsCount += 1
          if (tpe.typeParams exists (_.isAbstractType)) classesAtpCount += 1
          if (tpe.members exists (_.isAbstractType)) classesAtmCount += 1

          // Update the map thisEnclMethodToTypes 
          tpe.members filter (_.isMethod) foreach { method => thisEnclMethodToTypes(method) += tpe }

          // Get the package name
          packageNames += cls.pkg

          // Which class/method is that module defined in?
          // If it's defined in a class, add it to modulesInClass.
          // If in a method, add to the instantiated types in that method.
          if (cls.isModuleOrModuleClass) {
            // Look for main methods in top-level modules.
            if (enclMethodOrClass.isEmpty) {
              val main = tpe.members.filter { m =>
                m.isMethod && // consider only methods, not fields or other members
                  !m.isDeferred && // filter out abstract methods
                  definitions.isJavaMainMethod(m) // checks signature and name of method
              }.toList

              if (main.nonEmpty) {
                mainMethods += main.head
                mainModulesPrimaryConstructors += cls.primaryConstructor
                mainModules += tpe
              }
            }
          }
        }
      }
      case New(tpt) => {
        val tpe = tpt.tpe.dealias // Some types are aliased (see CaseClass3)

        // Do not process a type multiple times 
        if (!types(tpe)) {
          types += tpe
          if (tpe.typeSymbol.mixinClasses.nonEmpty) traitCompsCount += 1
        }
      }
      case _ => // don't care
    }

    /**
     * Traverse the children too, and look for modules in methods.
     * Modules can appear as one of the following trees:
     * - Select: e.g. val v = p(C.this.A) (so A is defined in Class/Module C)
     * - Ident: e.g. val v = p(A) (A is a top-level module)
     */
    def processChildren = {
      tree match {
        case _: Select | _: Ident if tree.symbol.isModuleOrModuleClass && !tree.symbol.isPackage =>
          if (enclMethod != NoSymbol) {
            //            if (tree.tpe.toString contains "TString") println("found in " + tree.getClass + " :: " + tree.tpe.getClass)
            addInstantiatedType(tree.tpe)
          }
        case _ =>
      }
      tree.children.foreach(traverse(_, tree :: ancestors))
    }

    // Karim: I think this method normalizes multiple parameter lists?
    def normalizeMultipleParameters(tree: Apply): Apply = tree.fun match {
      case a: Apply => normalizeMultipleParameters(a)
      case _ => tree
    }

    // Returns the receiver of an apply or unapply. Some don't have a receiver. This method never returns null.
    def getReceiver(tree: Tree): Option[Tree] = tree match {
      case a: Apply => getReceiver(a.fun)
      case s: Select => Some(s.qualifier)
      case t: TypeApply => getReceiver(t.fun)
      case i: Ident => Some(i)
      case _ => assert(assertion = false, message = "getReceiver on unexpected tree " + tree + " of type " + tree.getClass); null
    }

    // Filters out unwanted "method symbols" (e.g., labels)
    // Karim: we might not need all these checks now that we do the analysis after uncurry. 
    def isMethod(symbol: Symbol) = {
      symbol.isMethod
      symbol.isInstanceOf[MethodSymbol] &&
        symbol.name != nme.OUTER_SYNTH && // this is a fake method that never exists; such calls are
        // later replaced by calls to a synthetic accessor method
        !symbol.isLabel // gotos are implemented with a method-call-like syntax, but they do not call
      // actual methods, only labels
    }

    // look for an annotation on the receiver
    def findReceiverAnnotations(receiver: Tree) = {
      receiver match {
        case Block(annotations, plainRec) =>
          val annot = annotations.collect(annotationFilter)
          (annot.toSet, plainRec)
        case _ => (ImmutableSet.empty[String], receiver)
      }
    }

    // Add a new call site
    // TODO: the scala compiler sees String("A") of different type compared to String("B")
    def addCallSite(receiverTree: Tree, staticTarget: Symbol, enclMethod: Symbol, position: Position, annotations: ImmutableSet[String]) = {
      val abstractCallSite = CallSite(receiverTree, staticTarget)
      val callSite = CallSite(receiverTree, staticTarget, enclMethod, position, annotations)

      abstractCallSites += abstractCallSite
      callSitesInMethod(callSite.enclMethod) += abstractCallSite
      abstractToCallSites(abstractCallSite) += callSite

      if (abstractCallSite.hasModuleReceiver) {
        //        if (abstractCallSite.receiver.toString contains "TString") println("found in callSite " + abstractCallSite + " :: " + abstractCallSite.hasStaticSuperReference)
        addInstantiatedType(abstractCallSite.receiver)
      }
    }

    /**
     * Find the enclosing method from the given list of ancestors. For call sites in methods, that's obvious. For call sites
     * that appear in class definition, the primary constructor of the defining class is the enclosing method.
     */
    lazy val enclMethod = {
      ancestors.collectFirst {
        case cd: ClassDef => cd.symbol.primaryConstructor
        case dd: DefDef => dd.symbol
      }.getOrElse(NoSymbol)
    }

    /**
     * Return the first enclosing class (or object) or method.
     */
    lazy val enclMethodOrClass = {
      ancestors.find { parent =>
        parent match {
          case _: ClassDef | _: ModuleDef | _: DefDef => true
          case _ => false
        }
      }
    }

    // Find call sites in a class definition.
    // Adds the calls from primary constructors of classes to mixin constructors (see AbstractTypes13).
    def findCallSitesInClassDef(cls: ClassDef) = {
      //      if (!cls.symbol.isTrait) {
      for {
        mixin <- cls.symbol.mixinClasses
        caller = cls.symbol.primaryConstructor
        callee = mixin.primaryConstructor
        if caller != NoSymbol && callee != NoSymbol
        receiver = Super(This(caller.thisSym), tpnme.EMPTY) // calls to super constructors should be made via super
      } {
        /*
         * TODO: receiver.tpe can be null here (see SuperCall1), this is not problematic for method
         * resolution as we just get the static target. However, for checks like isSuper we need to add another
         * check that receiver != null
         */
        addCallSite(receiver, callee, caller, cls.pos, ImmutableSet.empty[String])
      }
      //      }

      findType(cls)
    }

    // Find call sites resulting from Apply(_, _)
    def findCallSitesInApply(apply: Apply) = {
      val a = normalizeMultipleParameters(apply)
      val callee = a.fun
      val args = a.args
      val receiver = getReceiver(a)
      if (isMethod(callee.symbol)) {
        val (annotations, plainReceiver) = findReceiverAnnotations(receiver.get)
        addCallSite(plainReceiver, apply.symbol, enclMethod, apply.pos, annotations)

        // If this is a call to asInstanceOf, add the type to the set of instantiateTypesInMethod
        if (definitions.isCastSymbol(apply.symbol) && apply.tpe.typeSymbol.isCaseClass) {
          //          if (apply.tpe.toString contains "TString") println("found in a cast expression :: " + apply.tpe.getClass)
          addInstantiatedType(apply.tpe)
        }
      }

      args foreach (traverse(_, apply :: ancestors))
      traverse(callee, apply :: ancestors) // TODO: This causes duplicate call sites (see foreach in Reachable1b)
    }

    // Find call sites resulting from Select(qual, name) or Ident(qual, name)
    def findCallSitesSelectOrIdent = {
      if (isMethod(tree.symbol)) {
        val receiver = getReceiver(tree)
        val (annotations, plainReceiver) = findReceiverAnnotations(receiver.get)
        addCallSite(plainReceiver, tree.symbol, enclMethod, tree.pos, annotations)
      }

      processChildren
    }

    // Find instantiated types.
    def findInstantiatedTypes(nw: New) = {
      //      if (nw.tpt.tpe.dealias.toString contains "TString") println("found in a new expression :: " + nw.tpt.tpe.dealias.getClass)
      addInstantiatedType(nw.tpt.tpe.dealias) // Some types are aliased (see CaseClass3)
      findType(nw)
    }

    // Find types and process children.
    def findType(tree: Tree) = {
      addType(tree)
      processChildren
    }
  }
}
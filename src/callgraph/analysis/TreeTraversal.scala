package callgraph.analysis

import collection.mutable
import output.CGAnnotations
import util.Global

trait TreeTraversal {

  this: Global with CGAnnotations =>

  import global._

  var types = Set[Type]()
  var callSites = List[CallSite]()
  val callSitesInMethod = mutable.Map[Symbol, Set[CallSite]]()
  var trees: List[Tree]

  case class CallSite(receiver: Tree, staticTarget: MethodSymbol, args: List[Tree], annotation: List[String],
                      ancestors: List[Tree], pos: Position, enclMethod: Symbol)

  // this is overridden by var trees in CallGraphPlugin
  def annotationFilter: PartialFunction[Tree, String]

  // todo (optimization): this method traverses the tree
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

  // todo (optimization): this method traverses the tree
  /* for every tree in the program, called only once from AbstractAnalysis.initialize */
  def findCallSitesAndTypes(tree: Tree, ancestors: List[Tree]) {

    tree match {
      case cls: ClassDef =>
        addType(cls.symbol.tpe)
        // Add the calls from primary constructors of classes to mixin constructors (see AbstractTypes13)
        findCallSitesClassDef(cls)
      case nw: New =>
        addType(nw.tpt.tpe)
        processChildren()
      // trees that invoke methods
      case apply: Apply =>
        findCallSitesApply(apply)
      case _: Select | _: Ident =>
        findCallSitesSelectOrIdent()
      case _ =>
        processChildren()
    }

    def addType(tpe: Type) {
      types += tpe
    }

    def addCallSite(callSite: CallSite) {
      callSites ::= callSite
      val enclMethod = callSite.enclMethod
      callSitesInMethod(enclMethod) = callSitesInMethod.getOrElse(enclMethod, Set()) + callSite
    }

    def findCallSitesClassDef(cls: ClassDef) {
      for {
        mixin <- cls.symbol.mixinClasses
        caller = cls.symbol.primaryConstructor
        callee = mixin.primaryConstructor
        if caller != NoSymbol && callee != NoSymbol
        receiver = This(caller.thisSym)
      } {
        addCallSite(CallSite(receiver, callee.asMethod, List[Tree](), List[String](), ancestors, tree.pos, caller))
      }
      processChildren()
    }

    def findCallSitesApply(apply: Apply) {
      val a = normalizeMultipleParameters(apply)
      val callee = a.fun
      val args = a.args
      val receiver = getReceiver(a)
      if (isMethod(callee.symbol)) {
        val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
        val _enclMethod = enclMethod(ancestors)
        addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, args, annotation, ancestors, tree.pos, _enclMethod))
      }
      args.foreach(findCallSitesAndTypes(_, tree :: ancestors))
      callee.children.foreach(findCallSitesAndTypes(_, tree :: ancestors))
    }

    def findCallSitesSelectOrIdent() {
      if (isMethod(tree.symbol)) {
        val receiver = getReceiver(tree)
        val (annotation, plainReceiver) = findReceiverAnnotations(receiver.getOrElse(null))
        val _enclMethod = enclMethod(ancestors)
        addCallSite(CallSite(plainReceiver, tree.symbol.asMethod, List(), annotation, ancestors, tree.pos, _enclMethod))
      }
      processChildren()
    }

    def processChildren() {
      tree.children.foreach(findCallSitesAndTypes(_, tree :: ancestors))
    }
  }

  private def normalizeMultipleParameters(tree: Apply): Apply = tree.fun match {
    case a: Apply => normalizeMultipleParameters(a)
    case _ => tree
  }

  private def isMethod(symbol: Symbol) =
    symbol.isInstanceOf[MethodSymbol] &&
      symbol.name != nme.OUTER_SYNTH && // this is a fake method that never exists; such calls are
      // later replaced by calls to a synthetic accessor method
      !symbol.isLabel // gotos are implemented with a method-call-like syntax, but they do not call
  // actual methods, only labels

  // todo (optimization): this method traverses the tree
  /** Returns the receiver of an apply or unapply. Some don't have a receiver. */
  private def getReceiver(tree: Tree): Option[Tree] = tree match {
    case a: Apply => getReceiver(a.fun)
    case s: Select => Some(s.qualifier)
    case t: TypeApply => getReceiver(t.fun)
    case _: Ident => None
    case _ => assert(assertion = false, message = "getReceiver on unexpected tree " + tree + " of type " + tree.getClass); null
  }

  /**
   * Find the enclosing method from the given list of ancestors. For call sites in methods, that's obvious. For call sites
   * that appear in class definition, the primary constructor of the defining class is the enclosing method.
   */
  private def enclMethod(ancestors: List[Tree]) = {
    ancestors.collectFirst {
      case cd: ClassDef => cd.symbol.primaryConstructor
      case dd: DefDef => dd.symbol
    }.get
  }

  // look for an annotation on the receiver
  private def findReceiverAnnotations(receiver: Tree): (List[String], Tree) = {
    val (annotation, plainReceiver) =
      receiver match {
        case Block(annotations, plainRec) =>
          val annot = annotations.collect(annotationFilter)
          (annot, plainRec)
        case _ => (List(), receiver)
      }
    (annotation, plainReceiver)
  }
}

package scala.print.plugin

trait ExtractChildren extends WithGlobal with CaseClassPrinter {

  import global._
  
  private def listChildrenRaw(tree: Tree): Seq[Tree] = tree match {
    
    // merge ClassDef + enclosed Template into one because template inside classes lack position
    case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
      println("case 1")
      tparams ++ parents.filter(hasPos(_)) ++ treatSelf(self) ++ body
       // parents without pos are AnyRef, or Serializable/Product for case classes
      
    // merge ModuleDef + enclosed Template into one because template inside classes lack position
    case ModuleDef(mods, name, Template(parents, self, body)) =>
      println("case 2")
      parents.filter(hasPos(_)) ++ treatSelf(self) ++ body

    case Template(parents, self, body) =>
      println("case 3")
      parents.filter(hasPos(_)) ++ treatSelf(self) ++ body
      
    // children of default package               (packageName.isEmpty does not work)
    case PackageDef(Ident(packageName), stats) if packageName.toString == "<empty>" =>
      println("case 4")
      stats
      
    case TypeDef(mods, name, tparams, TypeBoundsTree(lo, hi)) => 
      tparams ++ treatTypeBound(lo) ++ treatTypeBound(hi)

    case _ =>
      println("case 5")
      tree.children
  }
  
  private def treatSelf(self: ValDef): Seq[ValDef] = if (self.isEmpty) Seq() else Seq(self)

  // Type bounds without position are inferred Nothing or Any.
  // hasPos is better criterion than ==Nothing/Any, because Nothing/Any could have been written explicitly
  private def treatTypeBound(b: Tree): Seq[Tree] = if (hasPos(b)) Seq(b) else Seq()
  
  def hasPos(tree: Tree): Boolean = try {
    tree.pos.start < tree.pos.end
  } catch {
    case e: java.lang.UnsupportedOperationException => false
  }
  
  class BadPositionsException extends Exception

  def listChildren(tree: Tree): Seq[Tree] = {
    val c = listChildrenRaw(tree) filter {
      case DefDef(mods, name, tparams, vparamss, tp, rhs) => name.isEmpty
      case EmptyTree => false
      case t => true
    }
    val (withpos, nopos) = c.partition(hasPos(_))
    if (!nopos.isEmpty) {
      val noposStr = nopos.map(_.getClass.getName).mkString("[Trees without position of types ", ", ", "] ")
      println(s"In tree #${tree.id}: $noposStr")
      println(showCaseClass(tree))
      println(c.map(showCaseClass(_)).mkString(" --- "))
      println(tree)
      throw new BadPositionsException
    } else {
      val children = withpos.sortBy(_.pos.start)
      if (!checkChildrenPosInvariants(tree.pos.start, children, tree.pos.end)) {
        throw new BadPositionsException
      } else {
        children 
      }
    }
  }

  /**
   * Assumes that children is sorted by start position, and that all children have a start & end position.
   *  Checks that children are not outside parent and do not overlap each other
   */
  def checkChildrenPosInvariants(parentStartPos: Int, children: Seq[Tree], parentEndPos: Int): Boolean = {
    var good = true
    for (c <- children if c.pos.start < parentStartPos || c.pos.end > parentEndPos) {
      println(s"Tree with pos $parentStartPos..$parentEndPos has a child of type ${c.getClass.getName} with pos ${c.pos.start}..${c.pos.end}")
      println(s"Child = $c\n")
      good = false
    }

    if (!children.isEmpty) {
      for ((c1, c2) <- children zip children.tail if c1.pos.end > c2.pos.start) {
        println(s"Tree1 of type ${c1.getClass.getName} with pos ${c1.pos.start}..${c1.pos.end} overlaps with sibling Tree2 of type ${c2.getClass.getName} with pos ${c2.pos.start}..${c2.pos.end}")
        println(s"Tree1 = $c1\n")
        println(s"Tree2 = $c2\n")
        good = false
      }
    }
    good
  }

  private def patternMatchTemplate(tree: Tree): Unit = {
    tree match {
      case ClassDef(mods, name, tparams, impl) =>
      case PackageDef(packaged, stats) =>
      case ModuleDef(mods, name, impl) =>
      case vd @ ValDef(mods, name, tp, rhs) =>
      case dd @ DefDef(mods, name, tparams, vparamss, tp, rhs) =>
      case td @ TypeDef(mods, name, tparams, rhs) =>
      case LabelDef(name, params, rhs) =>
      case Import(expr, selectors) =>
      case Template(parents, self, body) =>
      case Block(stats, expr) =>
      case Match(selector, cases) =>
      case CaseDef(pat, guard, body) =>
      case Star(elem) =>
      case Bind(name, t) =>
      case Function(vparams, body) =>
      case Typed(expr, tp) =>
      case Apply(fun, vargs) =>
      case Super(This(qual), mix) =>
      case This(qual) =>
      case Select(qual @ New(tpe), name) =>
      case Select(qualifier, name) =>
      case id @ Ident(name) =>
      case l @ Literal(x) =>
      case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
      case SelectFromTypeTree(qualifier, selector) =>
      case CompoundTypeTree(templ) =>
      case AppliedTypeTree(tp, args) =>
      case ExistentialTypeTree(tpt, whereClauses) =>
      case tbt @ TypeBoundsTree(lo, hi) =>
      case emptyTree if emptyTree.toString == "<empty>" =>
      case tree =>
    }
  }

}
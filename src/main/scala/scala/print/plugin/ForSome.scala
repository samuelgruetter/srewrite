package scala.print.plugin

import scala.reflect.internal.util.OffsetPosition


trait ForSome extends CaseClassPrinter with WithGlobal {

  import global._

  private val debug: Boolean = false
  
  /**
   * Given a TypeDef such as e.g. `type T >: Bar <: Foo` which is used to bound an existentially quantified type,
   * is this TypeDef inside a `forSome` block (true) or was the bound expressed using a wildcard (false)?
   */
  def isWhereClauseInsideForSome(typeDef: Tree): Boolean = typeDef match {
    case TypeDef(mods, _, _, TypeBoundsTree(_, _)) => !mods.isSynthetic
    case _ => false
  }

  /**
   * tree: after parser tree
   */
  def forSomeToWildcard(tree: Tree, source: Array[String], cu: CompilationUnit): Unit = {
    def originalSource(t: Tree): String = if (hasPos(t)) {
      source.slice(t.pos.start, t.pos.end).mkString("")
    } else {
      "<no source>"
    }

    def rec(parent: Tree, tree: Tree): Unit = traceIndented(debug, tree.getClass.getSimpleName + " # " + tree + " # " + originalSource(tree)){
      tree match {
        case extyp @ ExistentialTypeTree(tp, whereClauses) if whereClauses.exists(isWhereClauseInsideForSome) =>
          // cu.echo(new OffsetPosition(cu.source, extyp.pos.start), s"start of ExistentialTypeTree")
          // cu.echo(new OffsetPosition(cu.source, extyp.pos.end), s"end of type with ExistentialTypeTree")
          cu.echo(new OffsetPosition(cu.source, extyp.pos.start), s"rewriting existential type using forSome")
          val newPrint: String = parent.toString
          // println(">>>" + newPrint + "<<<")
          // delete original:
          for (i <- extyp.pos.start until extyp.pos.end) source(i) = ""
          // insert the same type printed by scalac:
          source(extyp.pos.start) = newPrint
        case t: TypeTree if t.original != null => rec(t, t.original)
        case _ =>
      }
      for (c <- tree.children) rec(tree, c)
    }
    
    rec(null, tree)
  }

}

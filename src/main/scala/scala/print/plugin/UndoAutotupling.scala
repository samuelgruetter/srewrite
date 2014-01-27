package scala.print.plugin

import scala.collection.mutable.{ MultiMap, HashMap, Set }
import scala.reflect.internal.util.OffsetPosition

trait UndoAutotupling extends WithGlobal with ExtractChildren {

  import global._

  def undoAutotupling(afterParser: Tree, afterTyper: Tree, source: Array[String], cu: CompilationUnit): Unit = {
    val autotupled = scala.collection.mutable.Set[Tree]()
    markAutotupling(afterParser, afterTyper, t => autotupled.add(t))
    addTuplingParentheses(afterParser, source, t => autotupled.contains(t), cu)
  }

  /**
   * assuming tree is an after parser tree and has object Autotupled attached where needed
   *  @param source each String of length 1, can be used to add parentheses
   */
  def addTuplingParentheses(tree0: Tree, source: Array[String], isMarked: Tree => Boolean, cu: CompilationUnit): Unit = {

    def rec(tree: Tree): Unit = {
      // val children = listChildren(tree)
      val children = listChildrenWithoutPositionChecks(tree)
      if (isMarked(tree)) {
        //  snippet0 funcTree snip(pet1 argTree snip,pet2 argTree ... snip)petN
        val p1 = children(0).pos.end
        source(p1) = source(p1) + "("
        val p2 = tree.pos.end - 1
        source(p2) = source(p2) + ")"

        cu.echo(new OffsetPosition(cu.source, p1), "Inserting '(' here")
        cu.echo(new OffsetPosition(cu.source, p2), "Inserting ')' here")
      }

      for (c <- children) rec(c)
    }

    rec(tree0)
  }

  def showWithPos(tree: Tree): String = s"[(${tree.pos.start}) ${showCaseClass(tree)} (${tree.pos.end})]"

  def markAutotupling(afterParser: Tree, afterTyper: Tree, mark: Tree => Unit): Unit = {
    val m1 = allPosMap(afterParser)
    val m2 = allPosMap(afterTyper)
    val common = (m1.keys.toSet intersect m2.keys.toSet).toList.sorted
    for (pos <- common; t1 <- m1(pos); t2 <- m2(pos)) markAutotuplingOnOneTree(t1, t2, mark)
  }

  def markAutotuplingOnOneTree(afterParser: Tree, afterTyper: Tree, mark: Tree => Unit): Unit = {
    (afterParser, afterTyper) match {
      // unit might be a BoxedUnit, so we check using toString
      case (Apply(func1, Nil), Apply(func2, Literal(Constant(unit)) :: Nil)) if unit.toString == "()" => {
        // TODO check that func1 and func2 represent the same
        // reportReplacement(afterParser, afterTyper)
        // println(s"-> Autotupling of arity 0 detected\n")
        mark(afterParser)
      }
      case (Apply(func1, args1), Apply(func2, tupleConstr :: Nil)) => {
        // TODO check that func1 and func2 represent the same
        val arity = args1.length
        if (tupleConstr.toString.contains("Tuple" + arity)) {
          // reportReplacement(afterParser, afterTyper)
          // println(s"-> Autotupling of arity $arity detected\n")
          mark(afterParser)
        }
      }
      case _ =>
    }
  }

  def allPosMap(tree: Tree): MultiMap[Int, Tree] = {
    val mm = new HashMap[Int, Set[Tree]] with MultiMap[Int, Tree]
    traverse(tree) { t => { if (hasPos(t)) mm.addBinding(t.pos.start, t) } }
    mm
  }

}

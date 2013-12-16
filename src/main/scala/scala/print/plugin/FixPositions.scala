package scala.print.plugin

import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.RangePosition


trait FixPositions extends WithGlobal with ExtractChildren {

  import global._

  private def debug(msg: String): Unit = {} // println(msg)
  
  /**
   * If a tree has no position, but its children have a position, set the tree's
   * position such that it encloses its children, do so recursively.
   * 
   * NOT USED, because there's no case where it helps.
   * 
   * Originally intended to fix problems like these:
   * 
   * If user writes Function2[A, B], this results in AST
   * TypeApply(Ident(Function2), List(A, B)), then Function2 is replaced
   * by _root_.scala.Function2, which has no position.
   * Or in `def foo(t: (Int, Int)): Int = ...`, `(Int, Int)` is
   * rewritten to `scala.Tuple2[Int, Int]`, which has no position.  
   * 
   * However, Function2 and Tuple2 are of type Name, which is not a subclass
   * of Tree, so they don't have a position, and the fixPositions method
   * does not help.
   */
  def unused_fixPositions(tree: Tree, src: SourceFile): Unit = {
    debug(tree.id + " Start fixing pos of: " + tree)
    val children = listChildrenWithoutPositionChecks(tree)
    for (c <- children) {
      unused_fixPositions(c, src)
    }
    if (!hasPos(tree)) {
      val childrenWithPos = children.filter(hasPos(_))
      if (!childrenWithPos.isEmpty) {
        val minStart = childrenWithPos.map(_.pos.start).min
        val maxEnd = childrenWithPos.map(_.pos.end).max
        val pos = new RangePosition(src, minStart, minStart, maxEnd)
        tree.pos = pos
        debug(tree.id + " Fixed pos of " + tree)
      } else {
        // println instead of debug because quite serious
        println(tree.id + " [!!] Could not fix pos because no children with pos: " + tree)
      }
    } else {
      debug(tree.id + " No need to fix pos because already set: " + tree)
    }
  }
  
}

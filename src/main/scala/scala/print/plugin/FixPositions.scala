package scala.print.plugin

import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.RangePosition


trait FixPositions extends WithGlobal with ExtractChildren {

  import global._

  private def debug(msg: String): Unit = {} // println(msg)
  
  def fixPositions(tree: Tree, src: SourceFile): Unit = {
    debug(tree.id + " Start fixing pos of: " + tree)
    val children = listChildrenWithoutPositionChecks(tree)
    for (c <- children) {
      fixPositions(c, src)
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

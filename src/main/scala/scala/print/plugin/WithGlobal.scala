package scala.print.plugin

import scala.tools.nsc.Global

trait WithGlobal {
  val global: Global
  import global._

  /** action: (parent, child) => Unit */
  def traverse(tree: Tree)(action: (Tree, Tree) => Unit): Unit = {
    for (child <- tree.children) {
      action(tree, child)
      traverse(child)(action)
    }
  }
    
  def hasPos(tree: Tree): Boolean = try {
    tree.pos.start < tree.pos.end
  } catch {
    case e: java.lang.UnsupportedOperationException => false
  }

  var indent = 0

  def traceIndented[T](debug: Boolean, message: => String)(op: => T) = {
    if (debug) {
      val msg = message
      println("  " * indent + "==> " + msg)
      indent += 1
      op
      indent -= 1
      println("  " * indent + "<== " + msg)
    } else {
      op
    }
  }

}

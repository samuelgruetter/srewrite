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
}

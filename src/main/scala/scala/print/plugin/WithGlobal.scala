package scala.print.plugin

import scala.tools.nsc.Global

trait WithGlobal {
  val global: Global
  import global._
    
  def traverse(tree: Tree)(action: Tree => Unit): Unit = {
    action(tree)
    tree.children.foreach(t => traverse(t)(action))
  }
}

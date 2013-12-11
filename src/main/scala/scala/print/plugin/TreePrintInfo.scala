package scala.print.plugin

trait TreePrintInfo extends WithGlobal {
  
  def listChildren(tree: global.Tree): Seq[global.Tree] = {
    tree.children
  }
  
  
}
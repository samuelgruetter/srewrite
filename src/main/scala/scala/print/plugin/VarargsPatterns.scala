package scala.print.plugin

import scala.reflect.internal.util.OffsetPosition


trait VarargsPatterns extends CaseClassPrinter with WithGlobal {

  import global._

  private val debug: Boolean = false
  
  /**
   * tree: after parser tree
   */
  def varargsPatterns(tree: Tree, source: Array[String], cu: CompilationUnit): Unit = {
    def rec(tree: Tree): Unit = traceIndented(debug, tree.getClass.getSimpleName + " # " + tree) {
      tree match {
        case Bind(name, body @ Star(Ident(uscore))) if uscore.toString == "_"  && hasPos(tree) =>
          var p = body.pos.start
          while (source(p) != "@" && p > tree.pos.start) p -= 1
          if (source(p) != "@") {
            cu.error(new OffsetPosition(cu.source, body.pos.start), "Bug: Expected to find '@' in front of Bind")
          }
          cu.echo(new OffsetPosition(cu.source, p), s"Replacing '@' by ':'")
          source(p) = ":"
        case _ =>
      }
      for (c <- tree.children) rec(c)
    }
    
    rec(tree)

  }

}

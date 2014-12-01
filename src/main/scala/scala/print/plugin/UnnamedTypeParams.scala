package scala.print.plugin

import scala.reflect.internal.util.OffsetPosition
import scala.util.Random


trait UnnamedTypeParams extends CaseClassPrinter with WithGlobal {

  import global._

  private val debug: Boolean = false

  def freshTypeVarName: String = "X_" + math.abs((new Random).nextInt())

  /**
   * tree: after parser tree
   */
  def unnamedTypeParams(tree: Tree, source: Array[String], cu: CompilationUnit): Unit = {
    def treatTypeParamsList(tparams: List[TypeDef]): Unit = {
      for (td <- tparams if td.name.toString == "_" && hasPos(td)) {
        var p = td.pos.start
        while (source(p) != "_" && p < td.pos.end) p += 1
        if (source(p) != "_") {
          cu.error(new OffsetPosition(cu.source, td.pos.start), "Bug: Expected to find '_' in name of TypeDef")
        }
        cu.echo(new OffsetPosition(cu.source, p), "Replacing underscore by a fresh name")
        source(p) = freshTypeVarName
      }
    }
    
    def rec(tree: Tree): Unit = traceIndented(debug, tree.getClass.getSimpleName + " # " + tree) {
      tree match {
        case DefDef  (_, _, tparams: List[TypeDef], _, _, _) if tparams.exists(_.name.toString == "_")  => treatTypeParamsList(tparams)
        case ClassDef(_, _, tparams: List[TypeDef], _      ) if tparams.exists(_.name.toString == "_")  => treatTypeParamsList(tparams)
        case _ =>
      }
      for (c <- tree.children) rec(c)
    }
    
    rec(tree)

  }

}

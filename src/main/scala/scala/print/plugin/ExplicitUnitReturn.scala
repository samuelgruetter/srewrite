package scala.print.plugin

import scala.collection.mutable.{ MultiMap, HashMap, Set }
import scala.reflect.internal.util.OffsetPosition

trait ExplicitUnitReturn extends WithGlobal with ExtractChildren {

  import global._
  
  private def dbg(msg: String): Unit = {}
  //def dbg(msg: String): Unit = println(msg)
  
  /**
   * tree: after parser tree
   */
  def explicitUnitReturn(tree: Tree, source: Array[String], cu: CompilationUnit): Unit = {

    def forOneDefDef(dd: DefDef, source: Array[String]): Unit =
      if ((dd.tpt.toString == "Unit" || dd.tpt.toString == "scala.Unit") && 
          dd.name.toString != "$init$" &&
          dd.name.toString != "<init>" &&
          ! dd.rhs.isEmpty) {
        // hasPos requires that it spans strictly more than 0 chars, which is not the case for inserted Unit
        val l = dd.children.filter(hasPos(_)).sortBy(_.pos.start)
        dbg("name = " + dd.name)
        dbg("tpt = " + dd.tpt)
        dbg("children = " + dd.children)
        dbg("l = " + l)
        if (l.contains(dd.tpt)) {
          // Unit explicitly given by user, do nothing
        } else {
          // Unit not given by user
          // Note: dd.tpt.pos points to the name of the method, so we can't just insert ': Unit = ' at dd.tpt.pos
          val i = l.indexOf(dd.rhs)
          val start = if (i == 0) dd.pos.start else l(i-1).pos.end
          val end = dd.rhs.pos.start
          val sl = source.slice(start, end)
          dbg("slice = " + sl.mkString(""))
          if (!sl.contains("=")) {
            // procedure syntax, need to insert unit (TODO what if `=` in identifiers or comments?)
            var p = dd.rhs.pos.start
            
            if (source(dd.rhs.pos.start) != "{") {
              // Special case: if rhs is 1 statement surrounded by {}, the {} are removed by the parser and rhs.pos.start does
              // not point to '{', but to the first and only statement in {}
              // weird comments will crash us here
              if (sl.count(_ == "{") != 1) sys.error("expected exactly one '{' in " + sl.mkString(""))
              p = sl.indexOf("{") + start
            }
            while (source(p-1).head.isWhitespace) {
              p -= 1
            }
            val toInsert = if (source(p) == " ") ": Unit =" else ": Unit = "
            source(p) = toInsert + source(p)
            cu.echo(new OffsetPosition(cu.source, p), s"Inserting '$toInsert' here")
          }
        }
      }

    traverse(tree)(t => t match {
      case dd: DefDef => forOneDefDef(dd, source)
      case _ => // do nothing
    })
  }

  // Unit return type is inserted by parser (which does not make any symbols), so isSynthetic is false for inserted 
  // Unit return types

}

/*
import scala.tools.nsc.ast.parser.Scanners
    
trait MyScanners extends Scanners with WithGlobal {
  def getUniqueBracePos(s: String): Int = {
    val sc = new SourceStringScanner(s.toCharArray())
    sc.nextToken()
    sc.offset
    ???
  }
  
  import global._
  
  class SourceStringScanner(val source: Array[Char]) extends Scanner {
    val buf = source
    override val decodeUni: Boolean = !settings.nouescape.value

    // suppress warnings, throw exception on errors
    def warning(off: Offset, msg: String): Unit = ()
    def deprecationWarning(off: Offset, msg: String): Unit = ()
    def error  (off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
    def incompleteInputError(off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
  }
}
*/

package scala.print.plugin

import scala.collection.mutable.{ MultiMap, HashMap, Set }
import scala.reflect.internal.util.OffsetPosition

trait ExplicitImplicitTypes extends WithGlobal with ExtractChildren {

  import global._

  private def dbg(msg: String): Unit = println(msg)

  def explicitImplicitTypes(afterTyper: Tree, source: Array[String], cu: CompilationUnit): Unit = {

    def oneValOrDefDef(d: ValOrDefDef, source: Array[String]): Unit = {
      /*
      if (d.symbol.isImplicit) println(s"implicit: $d")
      // no real position means not given by user
      if (!hasPos(d.tpt)) {
        cu.echo(d.pos, s"tpt is here")
      }
      */
      // problem: implicit value x becomes non-implicit value x + implicit <stable> <accessor> def x
      // after typer

      if (d.symbol.isImplicit && !hasPos(d.tpt) && hasPos(d)) {
        // hasPos requires that it spans strictly more than 0 chars, which is not the case for inferred return types
        val l = d.children.filter(hasPos(_)).sortBy(_.pos.start)
        dbg("\n_____" + d)
        dbg("name = " + d.name)
        dbg("tpt = " + d.tpt)
        dbg("rhs = " + d.rhs)
        dbg("rhs.pos = " + d.rhs.pos)
        dbg("children = " + d.children)
        dbg("childrenPos = " + d.children.map(_.pos))
        dbg("l = " + l)

        // Note: d.tpt.pos points to the name of the method, so we can't just insert the return type at dd.tpt.pos
        val i = l.indexOf(d.rhs)
        val start = if (i == 0) d.pos.start else l(i - 1).pos.end
        val end = d.rhs.pos.start
        val sl = source.slice(start, end)
        dbg("slice = " + sl.mkString(""))

        var p = start + sl.indexOf("=")
        if (p < start) sys.error("no '=' found")
        if (source(p-1) == " ") p -= 1

        val toInsert = ": " + d.tpt.toString
        source(p) = toInsert + source(p)
        cu.echo(new OffsetPosition(cu.source, p), s"Inserting '$toInsert' here")
        /*
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
          while (source(p - 1).head.isWhitespace) {
            p -= 1
          }
          val toInsert = if (source(p) == " ") ": Unit =" else ": Unit = "
          source(p) = toInsert + source(p)
          cu.echo(new OffsetPosition(cu.source, p), s"Inserting '$toInsert' here")
        }
        */

      }
    }

    traverse(afterTyper)(t => t match {
      case d: ValOrDefDef => oneValOrDefDef(d, source)
      case _ => // do nothing
    })
  }
}

package scala.print.plugin

import scala.reflect.internal.util.OffsetPosition

trait ExplicitImplicitTypes extends WithGlobal with ExtractChildren {

  import global._

  private def dbg(msg: String): Unit = {}//println(msg)

  private def isImplicitAccessorFor(dd: DefDef, vd: ValDef): Boolean =
    dd.name.decode + " " == vd.name.decode && // yes, val name gets a space at the end!
      dd.symbol.isImplicit && dd.symbol.isStable && dd.symbol.isAccessor

  def explicitImplicitTypes(afterTyper: Tree, source: Array[String], cu: CompilationUnit): Unit = {

    def oneValOrDefDef(parent: Tree, d: ValOrDefDef, source: Array[String]): Unit = {

      // problem: implicit value x becomes non-implicit value x + implicit <stable> <accessor> def x
      val isValueWithImplicitAccessor = d match {
        case vd: ValDef => parent.children.exists(ia => (ia match {
          case dd: DefDef => isImplicitAccessorFor(dd, vd)
          case _ => false
        }))
        case _ => false
      }

      if (!hasPos(d.tpt) && hasPos(d) && (d.symbol.isImplicit || isValueWithImplicitAccessor) &&
          !d.rhs.isEmpty /*cases like List(1, 2) map {implicit x => x}*/) {
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
      }
    }

    traverse(afterTyper)((parent, child) => child match {
      case d: ValOrDefDef => oneValOrDefDef(parent, d, source)
      case _ => // do nothing
    })
  }
}

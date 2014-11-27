package scala.print.plugin

import scala.collection.mutable.{ MultiMap, HashMap, Set }
import scala.reflect.internal.util.OffsetPosition
import scala.reflect.internal.Flags._
import scala.collection.mutable

/** Comments out early initializers and puts a warning in source code and on console */
trait EarlyInitializers extends CaseClassPrinter with WithGlobal {

  import global._
  
  def isEarly(t: Tree): Boolean = t match {
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case TypeDef(mods: Modifiers, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  /*
  def firstNew(t: Tree): Option[New] = t match {
    case n: New => Some(n)
    case _ => t.children.iterator.flatMap(c => firstNew(c).toList).take(1).toList.headOption
  }*/
  
  /**
   * tree: after parser tree
   */
  def earlyInitializers(tree: Tree, source: Array[String], cu: CompilationUnit): Unit = {

    traverse(tree)((parent, child) => child match {
      case Block(ClassDef(_, _, _, templ @ Template(parents, _, templBody)) :: Nil, applyNew) if templBody.exists(isEarly) =>
        val start = templ.pos.start
        val end = parents.head.pos.start
        // cu.echo(new OffsetPosition(cu.source, start), s"start pos of Template")
        // cu.echo(new OffsetPosition(cu.source, end), s"start pos of first parent")
        cu.warning(new OffsetPosition(cu.source, start), "MANUAL CHANGE NEEDED for early initializers")
        source(start) = "/* TODO NEEDS MANUAL CHANGE (early initializers) " + source(start)
        source(end) = "*/ " + source(end)
      case _ => // do nothing
    })

    // Note: early ValDefs are duplicated by the parser so that they appear once in the template just as
    // a ValDef without initialization, and a second time inside the constructor, with initialization  

    /*
    val thoseWithEarlyChildren = new mutable.HashSet[Tree]

    traverse(tree)((parent, child) => if (isEarly(child)) parent match {
      // each early def appears once in the block of a constructor => don't take
      // and a second time in Template, => take
      case t: Template => thoseWithEarlyChildren += t
      case _ => // do nothing
    })

    traverse(tree)((parent, child) => if (isEarly(child)) parent match {
      // each early def appears once in the block of a constructor, that's when we take it
      // and a second time in Template, then we ignore it
      case b: Block => thoseWithEarlyChildren += b
      case _ => // do nothing
    })
    
    println("those with early children:\n")
    println(thoseWithEarlyChildren.mkString("\n\n"))

    for (t <- thoseWithEarlyChildren) {
      var start = t.filter(hasPos).map(_.pos.start).min
      var end = t.filter(hasPos).map(_.pos.end).max
      
      cu.echo(new OffsetPosition(cu.source, start), s"begin-pos")
      cu.echo(new OffsetPosition(cu.source, end), s"end-pos")
      
      while (source(start) != "{") start -= 1;
      
    }
    */
  }

}

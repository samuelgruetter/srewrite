package scala.print.plugin

trait CaseClassPrinter extends WithGlobal {

  def showCaseClass(t: global.Tree): String = {
    val clazz = t.getClass.getName.replaceFirst(".*\\$", "")
    val children: Iterator[String] = t.children match {
      case Nil => t.productIterator.map(_.toString)
      case l => l.map(showCaseClass(_)).iterator
    }
    children.mkString(clazz + "(", ", ", ")")
  } 
  
}
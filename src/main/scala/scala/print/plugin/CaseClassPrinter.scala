package scala.print.plugin

trait CaseClassPrinter extends WithGlobal {
  
  def showProduct(t: Any): String = {
    val clazz = t.getClass.getSimpleName //() .getName.replaceFirst("^.*?\\$", "") // lazy match from start until first $
    t match {
      case l: Iterable[Any] =>
        l.map(showProduct).mkString("{", ", ", "}")
      case t: Product =>
        t.productIterator.map(showProduct).mkString(clazz + "(", ", ", ")")
      case t =>
        val str = t.toString
        if (str contains clazz) str else s"$clazz[$str]"
    }
  }
  
  /*
  def showProduct(t: Product): String = {
    val clazz = t.getClass.getName.replaceFirst("^.*?\\$", "") // lazy match from start until first $
    val children: Iterator[String] = t.productIterator.map {
      case c: Product => showProduct(c)
      case c => c.toString 
    }
    children.mkString(clazz + "(", ", ", ")")
  }
  * 
  */
  
  def showCaseClass(t: global.Tree): String = {
    val clazz = t.getClass.getName.replaceFirst("^.*?\\$", "") // lazy match from start until first $
    val children: Iterator[String] = t.children match {
      case Nil => t.productIterator.map(_.toString)
      case l => l.map(showCaseClass(_)).iterator
    }
    children.mkString(clazz + "(", ", ", ")")
  }
}
package scala.print.plugin

trait CaseClassPrinter extends WithGlobal {

  def showCaseClass(t: global.Tree): String = {
    val clazz = t.getClass.getName.replaceFirst(".*\\$", "")
    val children = t.children.map(showCaseClass(_))
    children.mkString(clazz + "(", ", ", ")")
  } 
  
}
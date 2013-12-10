package scala.print.plugin

import java.io.{StringWriter, PrintWriter}
import scala.reflect.internal.Flags._
import scala.reflect.internal._
import scala.reflect.internal.util.SourceFile
import scala.collection.mutable

object FromSourceTreePrinter {
  def apply(compiler: Reflect) = {
    new FromSourceTreePrinter {
      val global = compiler
    }
  }
  
  type Reflect = SymbolTable
}

trait FromSourceTreePrinter {
  import FromSourceTreePrinter.Reflect
  val global: Reflect
  import global._

  def show(what: Reflect#Tree): String = {
    val xprinter = XTreePrinter(global)
    val x = xprinter.printer
    //val printer = new Printer(xprinter.out,
    ???
  }

  class Printer(out: PrintWriter, subPrinter: global.TreePrinter, printMultiline: Boolean = false, decodeNames: Boolean = true) extends global.TreePrinter(out) {

  }

}

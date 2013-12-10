package scala.print.plugin

import java.io.{StringWriter, PrintWriter}
import scala.reflect.internal.Flags._
import scala.reflect.internal._
import scala.reflect.internal.util.{SourceFile, Statistics}
import scala.collection.mutable

object SimpleRewriter{
  private[SimpleRewriter] trait PrinterDescriptor
  object AFTER_NAMER extends PrinterDescriptor
  object AFTER_TYPER extends PrinterDescriptor

  def apply(compiler: Reflect) = {
    new SimpleRewriter {
      val global = compiler
    }
  }

  type Reflect = SymbolTable
}

trait SimpleRewriter {

  import SimpleRewriter.Reflect
  val global: Reflect
  import global._

  //TODO set printMultiline for false
  def show(what: Reflect#Tree, printerType: SimpleRewriter.PrinterDescriptor = SimpleRewriter.AFTER_TYPER, printMultiline: Boolean = false, decodeNames: Boolean = false) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    var printer = printerType match {
      case SimpleRewriter.AFTER_NAMER => new PrettyPrinter(writer, printMultiline, decodeNames)
      case SimpleRewriter.AFTER_TYPER | _ => new AfterTyperPrinter(writer, printMultiline, decodeNames)
    }

    printer.print(what)
    writer.flush()
    buffer.toString
  }

  class PrettyPrinter(out: PrintWriter, printMultiline: Boolean = false, decodeNames: Boolean = true) extends global.TreePrinter(out) {

  }

  class AfterTyperPrinter(out: PrintWriter, printMultiline: Boolean = false, decodeNames: Boolean = false) extends PrettyPrinter(out, printMultiline, decodeNames) {

  }
  
}

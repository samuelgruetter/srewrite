package scala.print.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.ast.Printers
import java.io.{StringWriter, PrintWriter, File}
import scala.collection.mutable.ArrayBuffer

object SRewritePlugin {
  val baseDirectoryOpt = "base-dir:"
  val dirNameOpt = "dir-name:"
  val overSrcOpt = "oversrc"
}

class SRewritePlugin(val global: Global) extends Plugin {
  import SRewritePlugin._
  import global._

  val name = "srewriteplugin"
  val description = "WIP"

  var baseDir: String = System.getProperty("user.dir")
  var dirName = "sourceFromAST"
  var overrideSrc = false

  //object afterTyper extends PrintPhaseComponent("typer", "patmat")
  object afterParser extends PrintPhaseComponent("parser", "namer")

  val components = List[PluginComponent](afterParser)
  //val components = List[PluginComponent](afterTyper)

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.startsWith(baseDirectoryOpt)) {
        baseDir = option.substring(baseDirectoryOpt.length)
      } else if (option.startsWith(dirNameOpt)) {
        dirName = option.substring(dirNameOpt.length)
      } else if (option.endsWith(overSrcOpt)) {
        overrideSrc = true
      } else{
          error("Option not understood: "+option)
      }
    }
  }

  def writeSourceCode(unit: CompilationUnit, sourceCode: String, folderName: String) {

    def writeToFile(path: String, body: String, ovrride: Boolean = true) = {
      val file = new File(path)
        if (ovrride || !file.exists()) {
        val writer = new PrintWriter(file)
          try {
          writer.write(body)
        } finally {
          writer.close()
        }
      }
    }

    val defaultDirName = "sourceFromAST"
    val defaultDirPath = System.getProperty("user.dir")

    try {
      def findActualPath(filePath: String) =
        if (overrideSrc) filePath
          else filePath.replaceFirst(defaultDirPath, defaultDirPath + File.separator + dirName + File.separator + folderName)
            .replaceFirst(defaultDirPath, baseDir)

      val currentFile = unit.source.file.file
      Option(currentFile) map {
        currentFile =>
          val currentFilePath = currentFile.getAbsolutePath
          val actualPath = findActualPath(currentFilePath)
          println("currentFilePath: " + currentFilePath)
          println("actualPath: " + actualPath)

          //if parent file exists - create all dirs
          //currentFile.getParentFile.exists
          for (parentFile <- Option(currentFile.getParentFile)) {
            val parentFilePath = parentFile.getAbsolutePath
            val actualParentPath = findActualPath(parentFilePath)

            println("parentFilePath: " + parentFilePath)
            println("actualParentPath: " + actualParentPath)

            val dir = new File(actualParentPath)
            dir.mkdirs()
          }

          writeToFile(actualPath, sourceCode)

          //check file creation
          val checkFilePath = defaultDirPath + File.separator + ".checkSrcRegen"
          writeToFile(checkFilePath, "source regeneration: " + new java.util.Date(), false)
      } getOrElse {
        println("Can't process unit: " + unit)
//        for (ufile <- Option(unit.source.file)) {
//          for (ufilefile <- Option(ufile.file)) {
//            println("absolute path: " + ufilefile.getAbsolutePath)
//            for (ufilefileparent <- Option(ufilefile.getParentFile)) {
//              println("parent absolute path: " + ufilefileparent.getAbsolutePath)
//            }
//          }
//        }
      }
    } catch {
      case e @ _ => println("Error during processing unit: " + unit)
      throw e
    }
  }

  //Phase should be inserted between prevPhase and nextPhase
  //but it possible that not right after prevPhase or not right before nextPhase
  class PrintPhaseComponent(val prevPhase: String, val nextPhase: String) extends PluginComponent {
    val global: SRewritePlugin.this.global.type = SRewritePlugin.this.global

    override val runsAfter = List[String](prevPhase)
    override val runsBefore = List[String](nextPhase)

    //val printers = PrettyPrinters(global)

    val phaseName = "printSourceAfter_" + prevPhase
    def newPhase(_prev: Phase): StdPhase = new PrintPhase(_prev)

    class PrintPhase(prev: Phase) extends StdPhase(prev) {
      override def name = SRewritePlugin.this.name

      def apply(unit: CompilationUnit) {
        try {
            //regenerate only scala files
            val fileName = unit.source.file.name
            if (fileName.endsWith(".scala")) {
              val src: Array[Char] = unit.source.content
              
              println("-- Source name: " + fileName + " --")
              
              //val sourceCode = reconstructTree(unit.body)
              //val sourceCode = print4(unit.body, src)
              val sourceCode = print5(unit.body)
              
              writeSourceCode(unit, sourceCode, "before_" + nextPhase)
            } else
              println("-- Source name: " + fileName + " is not processed")
        } catch {
          case e: Exception =>
            e.printStackTrace()
            throw e
        }
      }
    }
    
    def print5(tree: Tree): String = {
      val r = Rewriter(global)
      r.show2(tree)
    }
    
    def print4(tree: Tree, source: Array[Char]): String = {
      def sourceStr(from: Int, to: Int) = String.valueOf(source, from, to-from)
      
      def print(tree: Tree): String = try {
        // all direct children sorted by position
        val children = extractChildren(tree)
        val pos = try {
          s"${tree.pos.start}..${tree.pos.end}"
        } catch {
          case e: java.lang.UnsupportedOperationException => "unknown"
        }
        
        s"\ntree #${tree.id} of type ${tree.getClass.getName} at position ${pos}:\n$tree\n" +
        children.map(print(_)).mkString("\n\n") 
      }
      
      print(tree)
    }
    
    def print3(tree: Tree, source: Array[Char]): String = {
      def sourceStr(from: Int, to: Int) = String.valueOf(source, from, to-from)
      
      val treesWithoutPos: ArrayBuffer[Tree] = ArrayBuffer()
      val treesWithPos: ArrayBuffer[Tree] = ArrayBuffer()
      
      def print(tree: Tree): String = try {
        // all direct children sorted by position
        val children: List[Tree] = extractChildren(tree).toList.sortBy(_.pos.start) // !!! .pos.start might be undefined!
        val codeStarts = tree.pos.start :: children.map(_.pos.end)
        val codeEnds = children.map(_.pos.start) ::: List(tree.pos.end)
        val codeSnippets = for ((s, e) <- codeStarts zip codeEnds) yield sourceStr(s, e)
        val childrenCode = "" :: children.map(print(_))
        val body = for ((child, snippet) <- childrenCode zip codeSnippets) yield child + snippet
        
        treesWithPos.append(tree)
        
        s"/*<<${tree.id}*/$body/*${tree.id}>>*/"
      } catch {
        case e: java.lang.UnsupportedOperationException => {
          treesWithoutPos.append(tree)
          s"/*SORRY, tree ${tree.id} has no start/end pos*/"
        }
      }
      
      print(tree) + 
        "\n### Trees without position:\n" + treesWithoutPos.map(dumpTree(_)).mkString("\n") +
        "\n### Trees with position:\n"       + treesWithPos.map(dumpTree(_)).mkString("\n")
    }
    
    def dumpTree(t: Tree): String = {
      s"tree #${t.id} of type ${t.getClass.getName}:\n$t\n"
    }

    def extractChildren(tree: Tree): Iterator[Tree] = {
      tree.productIterator.flatMap(e => e match {
        case t: Tree => List(t)
        case l: Iterable[_] => l.collect(new PartialFunction[Any, Tree] {
          def apply(x: Any): Tree = x.asInstanceOf[Tree]
          def isDefinedAt(x: Any): Boolean = x match {
            case t: Tree => true
            /*
            try {
              t.pos.start < t.pos.end 
            } catch {
              case e: Exception => false
            }
            */
            case _ => false
          }
        })
        case _ => List()
      })
    }
    
    def reconstructTree0(what: Tree): String = {
      //val rewriter = Rewriter(global)
      val rewriter = SimpleRewriter(global)
      "/* (begin code) */\n" +
      rewriter.show(what, SimpleRewriter.AFTER_NAMER, printMultiline = true) + 
      "\n/* (end code) */"
    }
    
    def reconstructTree1(what: Tree): String = {
      what.toString
    }
    
    def reconstructTree(what: Tree): String = {
      printIt(what, "")
    }
    
    def print2(t: Tree): String = {
      t.pos.source
      ???
    }
    
    def printIt(t: Product, indent: String): String = {
      val indentNew = indent + "\t"
      val body = (for (child <- t.productIterator if child != Nil) yield child match {
        case c: Product => printIt(c, indentNew)
        case _ => s"${indentNew}WEIRD-ANY($child)"
      }).mkString(",\n")
      
      sm"""|$indent${t.getClass.getName}(
           |$body
           |$indent)"""
    }
  }
}


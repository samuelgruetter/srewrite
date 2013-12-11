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
              val sourceCode = print6(unit.body, src)
              
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
    
    def hasPos(tree: Tree): Boolean = try {
      tree.pos.start < tree.pos.end
    } catch {
      case e: java.lang.UnsupportedOperationException => false
    }

    class OverlapException extends Exception
    
    def print6(tree: Tree, source: Array[Char]): String = {
      def sourceStr(from: Int, to: Int) = {
        println(s"${source.length}/$from/$to")
        String.valueOf(source, from, to-from)
      }
      
      def print(tree: Tree, treeStartPos: Int, treeEndPos: Int): String = {
        val (withpos, nopos) = tree.children.partition(hasPos(_))
        
        // all direct children sorted by position
        val children = withpos.sortBy(_.pos.start)

        for (c <- children if c.pos.start < treeStartPos || c.pos.end > treeEndPos) {
          println(s"Tree of type ${tree.getClass.getName} with pos $treeStartPos..$treeEndPos has a child of type ${c.getClass.getName} with pos ${c.pos.start}..${c.pos.end}")
          println(s"Tree = $tree\n")
          println(s"Child = $c\n")
        }
        
        if (!children.isEmpty) {
          for ((c1, c2) <- children zip children.tail if c1.pos.end > c2.pos.start) {
            println(s"Tree1 of type ${c1.getClass.getName} with pos ${c1.pos.start}..${c1.pos.end} overlaps with sibling Tree2 of type ${c2.getClass.getName} with pos ${c2.pos.start}..${c2.pos.end}")
            println(s"Tree1 = $c1\n")
            println(s"Tree2 = $c2\n")
            throw new OverlapException
          }
        }
        
        val codeStarts = treeStartPos :: children.map(_.pos.end)
        val codeEnds = children.map(_.pos.start) ::: List(treeEndPos)
        val codeSnippets = for ((s, e) <- codeStarts zip codeEnds) yield sourceStr(s, e)
        val childrenCode = "" :: children.map(c => print(c, c.pos.start, c.pos.end))
        val body = (for ((child, snippet) <- childrenCode zip codeSnippets) yield child + snippet).mkString("")
        
        val noposStr = if (nopos.isEmpty) "" else 
          nopos.map(_.getClass.getName).mkString("[Trees without position of types ", ", ", "] ") 
        
        if (!nopos.isEmpty) {
          println(s"In tree #${tree.id}: $noposStr")
        }
        //s"/*<<${tree.id}*/$body/*$noposStr${tree.id}>>*/"
        //body
        s"/*<<${tree.id}*/$body/*${tree.id}>>*/"
      }
      
      try {
        print(tree, 0, source.length)
      } catch {
        case e: OverlapException => "error"
      }
    }
    
  }
}


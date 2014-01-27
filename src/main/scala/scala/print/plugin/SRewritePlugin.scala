package scala.print.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.ast.Printers
import java.io.{StringWriter, PrintWriter, File}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.SourceFile

object SRewritePlugin {
  val baseDirectoryOpt = "base-dir:"
  val dirNameOpt = "dir-name:"
  val overSrcOpt = "oversrc"
}

class SRewritePlugin(val global: Global) extends Plugin with Transformations {
  import SRewritePlugin._
  import global._

  val name = "srewriteplugin"
  val description = "WIP"

  var baseDir: String = System.getProperty("user.dir")
  var dirName = "sourceFromAST"
  var overrideSrc = false

  object afterParser extends AfterParserPhaseComponent("parser", "namer")
  object afterTyper extends AfterTyperPhaseComponent("typer", "patmat")

  val components = List[PluginComponent](afterParser, afterTyper)

  val fileToAfterParserTree = scala.collection.mutable.Map[scala.reflect.io.AbstractFile, global.Tree]()
  
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
      }
    } catch {
      case e : Throwable => println("Error during processing unit: " + unit)
      throw e
    }
  }

  class AfterParserPhaseComponent(val prevPhase: String, val nextPhase: String) extends PluginComponent {
    val global: SRewritePlugin.this.global.type = SRewritePlugin.this.global

    override val runsAfter = List[String](prevPhase)
    override val runsBefore = List[String](nextPhase)

    val phaseName = "Grab after-parser tree"
    def newPhase(_prev: Phase): StdPhase = new GrabTreePhase(_prev)

    class GrabTreePhase(prev: Phase) extends StdPhase(prev) {
      override def name = SRewritePlugin.this.name

      def apply(unit: CompilationUnit) {
        try {
            //regenerate only scala files
            val fileName = unit.source.file.name
            if (fileName.endsWith(".scala")) {
              
               // looks like duplicate does not copy position
              fileToAfterParserTree.update(unit.source.file, unit.body/*.duplicate*/)
              
              println("-- Source name: " + fileName + " --")
              
            } else
              println("-- Source name: " + fileName + " is not processed")
        } catch {
          case e: Exception =>
            e.printStackTrace()
            throw e
        }
      }
    }
  }
  
  class AfterTyperPhaseComponent(val prevPhase: String, val nextPhase: String) extends PluginComponent {
    val global: SRewritePlugin.this.global.type = SRewritePlugin.this.global

    override val runsAfter = List[String](prevPhase)
    override val runsBefore = List[String](nextPhase)

    val phaseName = "Undo autotupling and print source code"
    def newPhase(_prev: Phase): StdPhase = new UndoAutotuplingPhase(_prev)

    class UndoAutotuplingPhase(prev: Phase) extends StdPhase(prev) {
      override def name = SRewritePlugin.this.name

      def apply(unit: CompilationUnit) {
        try {
            //regenerate only scala files
            val fileName = unit.source.file.name
            
            if (fileName.endsWith(".scala")) {            
              val afterParserTree = fileToAfterParserTree(unit.source.file)
              val afterTyperTree = unit.body
              
              // release stuff:
              fileToAfterParserTree.remove(unit.source.file)
              
              println("-- Source name: " + fileName + " --")
              
              val sourceCode = transformations(afterParserTree, afterTyperTree, unit)
              
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
  }

}


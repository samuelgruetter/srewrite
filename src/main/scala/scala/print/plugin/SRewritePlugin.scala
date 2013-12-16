package scala.print.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.ast.Printers
import java.io.{StringWriter, PrintWriter, File}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{MultiMap, HashMap, Set}
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.OffsetPosition

object SRewritePlugin {
  val baseDirectoryOpt = "base-dir:"
  val dirNameOpt = "dir-name:"
  val overSrcOpt = "oversrc"
}

class SRewritePlugin(val global: Global) extends Plugin with CaseClassPrinter with ExtractChildren with FixPositions {
  import SRewritePlugin._
  import global._

  val name = "srewriteplugin"
  val description = "WIP"

  var baseDir: String = System.getProperty("user.dir")
  var dirName = "sourceFromAST"
  var overrideSrc = false

  object afterParser extends PrintPhaseComponent("parser", "namer")
  object afterTyper extends AfterTyperPhaseComponent("typer", "patmat")

  val components = List[PluginComponent](afterParser, afterTyper)
  //val components = List[PluginComponent](afterTyper)

  val fileToAfterParserTree = scala.collection.mutable.Map[scala.reflect.io.AbstractFile, global.Tree]()
  val fileToAfterParserSource = scala.collection.mutable.Map[scala.reflect.io.AbstractFile, Array[Char]]()
  
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

  class PrintPhaseComponent(val prevPhase: String, val nextPhase: String) extends PluginComponent {
    val global: SRewritePlugin.this.global.type = SRewritePlugin.this.global

    override val runsAfter = List[String](prevPhase)
    override val runsBefore = List[String](nextPhase)

    val phaseName = "printSourceAfter_" + prevPhase
    def newPhase(_prev: Phase): StdPhase = new PrintPhase(_prev)

    class PrintPhase(prev: Phase) extends StdPhase(prev) {
      override def name = SRewritePlugin.this.name

      def apply(unit: CompilationUnit) {
        try {
            //regenerate only scala files
            val fileName = unit.source.file.name
            if (fileName.endsWith(".scala")) {
              
              fileToAfterParserTree.update(unit.source.file, unit.body/*.duplicate*/) // looks like duplicate does not copy position
              fileToAfterParserSource.update(unit.source.file, unit.source.content)
              
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

    val phaseName = "printSourceAfter_" + prevPhase
    def newPhase(_prev: Phase): StdPhase = new PrintPhase2(_prev)

    class PrintPhase2(prev: Phase) extends StdPhase(prev) {
      override def name = SRewritePlugin.this.name

      def apply(unit: CompilationUnit) {
        try {
            //regenerate only scala files
            val fileName = unit.source.file.name
            if (fileName.endsWith(".scala")) {
            
              val tree = fileToAfterParserTree(unit.source.file)
              val src: Array[Char] = fileToAfterParserSource(unit.source.file)
              
              // release stuff:
              fileToAfterParserTree.remove(unit.source.file)
              fileToAfterParserSource.remove(unit.source.file)
              
              println("-- Source name: " + fileName + " --")
              
              val sourceCode = utils.print10(tree, unit.body, unit)
              
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

  /** can be attached to a tree to indicate that the tree should be overridden by this new tree */
  case class NewTree(tree: Tree)
  
  object utils {
    
    def print10(afterParser: Tree, afterTyper: Tree, cu: CompilationUnit): String = {
      val autotupled = scala.collection.mutable.Set[Tree]()
      markAutotupling(afterParser, afterTyper, t => autotupled.add(t))
      val source2 = cu.source.content.map(String.valueOf(_))
      addTuplingParentheses(afterParser, source2, t => autotupled.contains(t), cu)
      source2.mkString
    }

    /** assuming tree is an after parser tree and has object Autotupled attached where needed 
     *  @param source each String of length 1, can be used to add parentheses */
    def addTuplingParentheses(tree0: Tree, source: Array[String], isMarked: Tree => Boolean, cu: CompilationUnit): Unit = {
      
      def rec(tree: Tree): Unit = {
        // val children = listChildren(tree)
        val children = listChildrenWithoutPositionChecks(tree)
        if (isMarked(tree)) {
          //  snippet0 funcTree snip(pet1 argTree snip,pet2 argTree ... snip)petN
          val p1 = children(0).pos.end
          source(p1) = "(" + source(p1)
          val p2 = tree.pos.end - 1
          source(p2) = source(p2) + ")"

          cu.echo(new OffsetPosition(cu.source, p1), "Inserting '(' here")
          cu.echo(new OffsetPosition(cu.source, p2), "Inserting ')' here")
        }
        
        for (c <- children) rec(c)
      }
      
      rec(tree0)      
    }

    def showWithPos(tree: Tree): String = s"[(${tree.pos.start}) ${showCaseClass(tree)} (${tree.pos.end})]"
    
    def markAutotupling(afterParser: Tree, afterTyper: Tree, mark: Tree => Unit): Unit = {
      val m1 = allPosMap(afterParser)
      val m2 = allPosMap(afterTyper)
      val common = (m1.keys.toSet intersect m2.keys.toSet).toList.sorted
      for (pos <- common; t1 <- m1(pos); t2 <- m2(pos)) markAutotuplingOnOneTree(t1, t2, mark)
    }
    
    def markAutotuplingOnOneTree(afterParser: Tree, afterTyper: Tree, mark: Tree => Unit): Unit = {
      (afterParser, afterTyper) match {
        // unit might be a BoxedUnit, so we check using toString
        case (Apply(func1, Nil), Apply(func2, Literal(Constant(unit)) :: Nil)) if unit.toString == "()" => {
          // TODO check that func1 and func2 represent the same
          // reportReplacement(afterParser, afterTyper)
          // println(s"-> Autotupling of arity 0 detected\n")
          mark(afterParser)
        }
        case (Apply(func1, args1), Apply(func2, tupleConstr :: Nil)) => { 
          // TODO check that func1 and func2 represent the same
          val arity = args1.length
          if (tupleConstr.toString.contains("Tuple" + arity)) {
            // reportReplacement(afterParser, afterTyper)
            // println(s"-> Autotupling of arity $arity detected\n")
            mark(afterParser)
          }
        }
        case _ =>
      }
    }

    def traverse(tree: Tree)(action: Tree => Unit): Unit = {
      action(tree)
      tree.children.foreach(t => traverse(t)(action))
    }
    
    def allPosMap(tree: Tree): MultiMap[Int, Tree] = {
      val mm = new HashMap[Int, Set[Tree]] with MultiMap[Int, Tree]
      traverse(tree){t => {if (hasPos(t)) mm.addBinding(t.pos.start, t)}}
      mm
    }
  }
}


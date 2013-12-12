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

object SRewritePlugin {
  val baseDirectoryOpt = "base-dir:"
  val dirNameOpt = "dir-name:"
  val overSrcOpt = "oversrc"
}

class SRewritePlugin(val global: Global) extends Plugin with CaseClassPrinter {
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
      case e : Throwable => println("Error during processing unit: " + unit)
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
  
  //Phase should be inserted between prevPhase and nextPhase
  //but it possible that not right after prevPhase or not right before nextPhase
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
              
              utils.compare3(tree, unit.body)
              
              //val sourceCode = utils.print6(tree, src)
              //val sourceCode = utils.print6(unit.body, src)
              val sourceCode = utils.print7(tree) // print7 only possible with before-typer-tree
              
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

  object utils {
    
    
    def traverse(tree: Tree)(action: Tree => Unit): Unit = {
      action(tree)
      tree.children.foreach(t => traverse(t)(action))
    }
    
    def allPosMap(tree: Tree): MultiMap[Int, Tree] = {
      val mm = new HashMap[Int, Set[Tree]] with MultiMap[Int, Tree]
      traverse(tree){t => {if (hasPos(t)) mm.addBinding(t.pos.start, t)}}
      mm
    }
    
    def compare3(oldTree: Tree, newTree: Tree): Unit = {
      val m1 = allPosMap(oldTree)
      val m2 = allPosMap(newTree)
      val common = (m1.keys.toSet intersect m2.keys.toSet).toList.sorted
      for (pos <- common; t1 <- m1(pos); t2 <- m2(pos)) compareOne(t1, t2)
    }
    
    // no recursion
    def compareOne(oldTree: Tree, newTree: Tree): Unit = {
      
      (oldTree, newTree) match {
        // unit might be a BoxedUnit, so we check using toString
        case (Apply(func1, Nil), Apply(func2, Literal(Constant(unit)) :: Nil)) if unit.toString == "()" => {
          // TODO check that func1 and func2 represent the same
          reportReplacement(oldTree, newTree)
          println(s">>> Autotupling of arity 0 detected\n")
        }
        case (Apply(func1, args1), Apply(func2, tupleConstr :: Nil)) => { 
          // TODO check that func1 and func2 represent the same
          val arity = args1.length
          if (tupleConstr.toString.contains("Tuple" + arity)) {
            reportReplacement(oldTree, newTree)
            println(s">>> Autotupling of arity $arity detected\n")
          }
        }
        case _ =>
      }
      
      /*
       * Replacement: #91: Apply(Ident())
       * f()
       * by #489: Apply(Select(This()), Literal())
       * AutoTuplingTest.this.f(())
       * 
       */
    }
    
    // returns a multi-map (pos -> list of all *direct* children with this pos)
    def posMap(t: Tree): Map[Int, List[Tree]] = {
      val l = t.children.collect(new PartialFunction[Tree, (Int, Tree)] { 
        def apply(x: Tree) = (x.pos.start, x)
        def isDefinedAt(x: Tree): Boolean = hasPos(x)
      })
      l.groupBy(p => p._1).map(p => (p._1, p._2.map(_._2)))
    }
    
    def compare2(oldTree: Tree, newTree: Tree): Unit = {
      //if (oldTree.getClass.getName != newTree.getClass.getName) reportReplacement(oldTree, newTree)
      /*if (oldTree.id != newTree.id)*/ reportReplacement(oldTree, newTree)
      
      (oldTree, newTree) match {
        case (Apply(fun, args), Block(stat :: stats, expr)) => stat match {
          case Apply(fun2, Apply(tupleConstr, args2) :: Nil) => 
            if (tupleConstr.toString.contains("Tuple")) {
              val n = Integer.parseInt(tupleConstr.toString.replaceFirst(".*Tuple", "").takeWhile(_.isDigit))
              println(s">>> Autotupling of arity $n detected")
            }
          case _ => 
        }
        case _ =>
      }
      /*
      Apply
		foo(3, 4)
		by 380 of type scala.reflect.internal.Trees$Block
		{
		  AutoTuplingTest.this.foo(scala.this.Tuple2.apply[Int, Int](3, 4));
		  ()
		}
      */
      
      (oldTree, newTree) match {
        case (Apply(func1, args1), Apply(func2, tupleConstr :: Nil)) => { 
          // TODO check that func1 and func2 represent the same
          val arity = args1.length
          if (tupleConstr.toString.contains("Tuple" + arity)) {
            println(s">>> Autotupling of arity $arity detected")
          }
        }
        case _ =>
      }
      
      /*
      Replacement: 108 of type scala.reflect.internal.Trees$Apply
	  fooo(4, 5, 6)
      by 521 of type scala.reflect.internal.Trees$Apply
      AutoTuplingTest.this.fooo(scala.this.Tuple3.apply[Int, Int, Int](4, 5, 6))
      */
      
      val m1 = posMap(oldTree)
      val m2 = posMap(newTree)
      val common = (m1.keys.toSet intersect m2.keys.toSet).toList.sorted
      for (pos <- common; t1 <- m1(pos); t2 <- m2(pos)) compare2(t1, t2)
    }
    
    def compare(oldTree: Tree, newTree: Tree): Unit = {
      if (oldTree.getClass.getName == newTree.getClass.getName && oldTree.children.length == newTree.children.length) {
        for ((o, n) <- oldTree.children zip newTree.children) {
          compare(o, n)
        }
      } else {
        reportReplacement(oldTree, newTree)
      }
    }
    
    def reportReplacement(oldTree: Tree, newTree: Tree): Unit = {
      println(s"\nReplacement: #${oldTree.id}: ${showCaseClass(oldTree)}")
      println(oldTree)
      println(s"by #${newTree.id}: ${showCaseClass(newTree)}")
      println(newTree)
      println()
    }
    
    def reportReplacement0(oldTree: Tree, newTree: Tree): Unit = {
      println(s"\nReplacement: ${oldTree.id} of type ${oldTree.getClass.getName}")
      println(oldTree)
      println(s"by ${newTree.id} of type ${newTree.getClass.getName}")
      println(newTree)
      println()
    }
    
    def hasPos(tree: Tree): Boolean = try {
      tree.pos.start < tree.pos.end
    } catch {
      case e: java.lang.UnsupportedOperationException => false
    }

    class OverlapException extends Exception
    
    def print7(tree: Tree): String = {
      val r = Rewriter(global)
      r.show20(tree.asInstanceOf[r.global.Tree])
    }
    
    def print6(tree: Tree, source: Array[Char]): String = {
      def sourceStr(from: Int, to: Int) = {
        //println(s"${source.length}/$from/$to")
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


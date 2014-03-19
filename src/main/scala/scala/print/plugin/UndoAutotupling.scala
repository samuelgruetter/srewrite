package scala.print.plugin

import scala.collection.mutable.{ MultiMap, HashMap, Set }
import scala.reflect.internal.util.OffsetPosition

trait UndoAutotupling extends WithGlobal with ExtractChildren {

  import global._

  private def dbg(msg: String): Unit = println(msg)

  def undoAutotupling(afterParser: Tree, afterTyper: Tree, source: Array[String], cu: CompilationUnit): Unit = {
    val autotupled = scala.collection.mutable.Set[Tree]()
    markAutotupling(afterParser, afterTyper, t => autotupled.add(t))
    addTuplingParentheses(afterParser, source, t => autotupled.contains(t), cu)
  }

  /**
   * assuming tree is an after parser tree and has object Autotupled attached where needed
   *  @param source each String of length 1, can be used to add parentheses
   */
  def addTuplingParentheses(tree0: Tree, source: Array[String], isMarked: Tree => Boolean, cu: CompilationUnit): Unit = {

    def rec(tree: Tree): Unit = {
      // val children = listChildren(tree)
      val children = listChildrenWithoutPositionChecks(tree)
      if (isMarked(tree)) {
        //  snippet0 funcTree snip(pet1 argTree snip,pet2 argTree ... snip)petN
        val p1 = children(0).pos.end
        source(p1) = source(p1) + "("
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
    val common = (m1.keys.toSet intersect m2.keys.toSet).toList.sortBy(_.startOrPoint)
    for (pos <- common; t1 <- m1(pos); t2 <- m2(pos)) markAutotuplingOnOneTree(t1, t2, mark)
  }

  def markAutotuplingOnOneTree(afterParser: Tree, afterTyper: Tree, mark: Tree => Unit): Unit = {
    (afterParser, afterTyper) match {
      // unit might be a BoxedUnit, so we check using toString
      case (Apply(func1, Nil), Apply(func2, Literal(Constant(unit)) :: Nil)) if unit != null && unit.toString == "()" => {
        // TODO check that func1 and func2 represent the same
        reportReplacement(afterParser, afterTyper)
        dbg(s"-> Autotupling of arity 0 detected\n")
        mark(afterParser)
      }
      // case (Apply(func1, args1), Apply(func2, tupleConstr :: Nil)) => {
      case (Apply(func1, args1), Apply(func2, Apply(tupleConstrFunc, args2) :: Nil)) => {
        // TODO check that func1 and func2 represent the same
        val arity = args1.length
        /*
        println(args1.length + " and " + args2.length)
        reportReplacement(afterParser, afterTyper)
        println("^^^^")
        *//*
        if (tupleConstr.toString.contains("Tuple" + arity + ".apply")
           && ! func1.toString.contains("Tuple" + arity) // <- don't do anything if implicit conversion applied to a tuple
        )*/
        if (args2.length == args1.length &&
            isTupleConstrFunc(tupleConstrFunc, arity) &&
            ! isTupleConstrFunc(func1, arity)
        ) {
          // println("-----" + tupleConstr.toString)
          reportReplacement(afterParser, afterTyper)
          dbg(s"-> Autotupling of arity $arity detected\n")
          mark(afterParser)
        }
      }
      case _ =>
    }
  }

  def isTupleConstrFunc(t: Tree, arity: Int): Boolean = t match {
    // With explicit type parameters for tuple construction function:
    // blahprefix.Tuple3.apply[T1, T2, T3]
    case TypeApply(Select(Select(blah, tupleName), applyName), types)
        if tupleName.encoded == "Tuple" + arity && applyName.encoded == "apply" => true
    // blahprefix.Tuple3[T1, T2, T3]
    case TypeApply(Select(blah, tupleName), types)
        if tupleName.encoded == "Tuple" + arity => true

    // Without explicit type parameters:
    // blahprefix.Tuple3.apply
    case Select(Select(blah, tupleName), applyName)
    	if tupleName.encoded == "Tuple" + arity && applyName.encoded == "apply" => true
    // blahprefix.Tuple3
    case Select(blah, tupleName)
        if tupleName.encoded == "Tuple" + arity => true

    case _ => false
  }


  def reportReplacement(tree1: Tree, tree2: Tree): Unit = {
    dbg("----old:----")
    dbg(tree1.toString)
    dbg(showCaseClass(tree1))
    dbg("----new:----")
    dbg(tree2.toString)
    dbg(showCaseClass(tree2))
  }

  def allPosMap(tree: Tree): MultiMap[Position, Tree] = {
    val mm = new HashMap[Position, Set[Tree]] with MultiMap[Position, Tree]
    traverse(tree) { (parent, child) => { if (hasPos(child)) mm.addBinding(child.pos, child) } }
    mm
  }

}

package com.vladimir.nik.print.plugin.printer

import com.vladimir.nik.print.plugin.PrintPlugin
import java.io.{StringWriter, PrintWriter}
import scala.reflect.internal.Flags._
import scala.tools.nsc
import scala.tools.nsc.ast.Printers
import nsc.Global

/**
 * Created with IntelliJ IDEA.
 * User: vova
 * Date: 7/15/13
 * Time: 2:38 AM
 * To change this template use File | Settings | File Templates.
 */

class ASTPrinters(val global: Global, val out: PrintWriter) {

  import global._

  //TODO refactor show
  //TODO not optimal - bad solution
  //don't forget this method after ASTPrinter moving!!!
  def show(what: Any, generic: Boolean) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    val printers = new ASTPrinters(global, writer)
    var printer = new printers.ASTPrinter


    printer.print(what)
    writer.flush()
    buffer.toString
  }

  class ASTPrinter extends global.TreePrinter(out) {
    //TODO maybe we need to pass this stack when explicitly run show inside print
    val contextStack = scala.collection.mutable.Stack[Tree]()

    //this methods are here because they are private
    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null | NoSymbol => orElse
      case sym => f(sym)
    }

    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    override def printModifiers(tree: Tree, mods: Modifiers): Unit = printModifiers(tree, mods, false)

    def printModifiers(tree: Tree, mods: Modifiers, isCtr: Boolean): Unit =
      if (modsAccepted)
      printFlags(
      if (tree.symbol == NoSymbol) mods.flags else tree.symbol.flags, "" + (
        if (tree.symbol == NoSymbol) mods.privateWithin
        else if (tree.symbol.hasAccessBoundary) tree.symbol.privateWithin.name
        else ""
        ), isCtr
      //we need to print implicits independently of context
    ) else {
        //TODO refactor
        if(mods.hasFlag(IMPLICIT)) printFlags(IMPLICIT, "", isCtr)
        if(mods.hasFlag(CASE)) printFlags(CASE, "", isCtr)
        if(mods.hasFlag(LAZY)) printFlags(LAZY, "", isCtr)
      }
    //TODO create else branch and if Lazy, and case

    def modsAccepted = getCurrentContext() match {
      case _:ClassDef | _:ModuleDef | _:Template | _:PackageDef => true
      case _ => false
    }

    //to skip all annotations
    override def printFlags(flags: Long, privateWithin: String) {
      printFlags(flags, privateWithin, false)
    }

    def printFlags(flags: Long, privateWithin: String, isCtr: Boolean) {
      //TODO parse ABSOVERRIDE
      val base = PROTECTED | OVERRIDE | PRIVATE | ABSTRACT | FINAL | SEALED | LAZY | LOCAL
      val ASTPrinterFlags = if (isCtr) base else base | IMPLICIT

      var mask: Long = if (settings.debug.value) -1L else ASTPrinterFlags
      //var mask: Long = if (settings.debug.value) -1L else PrintableFlags
      //var mask: Long = 0
      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") print(s + " ")
      //case flag should be the last
      val caseFlag = flagsToString(flags & CASE)
      if (caseFlag != "") print(caseFlag + " ")
      val absOverrideFlag = flagsToString(flags & ABSOVERRIDE)
      if (absOverrideFlag != "") print("abstract override ")
    }

    def printConstrParams(ts: List[ValDef], isConstr: Boolean) {
      print("(")
      if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
      printSeq(ts) {
        x => printParam(x, true)
      } {
        print(", ")
      }
      print(")")
    }

    override def printValueParams(ts: List[ValDef]) {
      printValueParams(ts, false)
    }

    def printValueParams(ts: List[ValDef], isFuncTree: Boolean) {
      //val a: Int => Int = implicit x => x //there shouldn't be paranthesis
      val printParanthesis = !isFuncTree || {
        ts match {
          case List(vd: ValDef) => !vd.mods.hasFlag(IMPLICIT)
          case _ => true
        }
      }

      if (printParanthesis) print("(")
      //TODO when we have constructor with implicit params first group of parameters
      //always are not implicits - even if there are no other parameters ()(implicit val a:String ...)
      if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
      printSeq(ts) {
        printParam
      } {
        print(", ")
      }
      if (printParanthesis) print(")")
    }

    def printParam(tree: Tree, isConstr: Boolean) {
      tree match {
        case ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(tree)
          if (isConstr) {
            printModifiers(tree, mods, isConstr)
          }
          print(if (mods.isMutable && isConstr) "var " else if (isConstr) "val " else "", symName(tree, name));
          if (name.endsWith("_")) print(" ");
          printOpt(": ", tp);
          printOpt(" = ", rhs)
        case TypeDef(mods, name, tparams, rhs) =>
          printPosition(tree)
          print(symName(tree, name))
          printTypeParams(tparams);
          print(rhs)
      }
    }

    override def printParam(tree: Tree) {
      printParam(tree, false)
    }

    override def printAnnotations(tree: Tree) {
      // SI-5885: by default this won't print annotations of not yet initialized symbols
      //    val annots = tree.symbol.annotations match {
      //      case Nil  => tree.asInstanceOf[MemberDef].mods.annotations
      //      case anns => anns
      //    }

      val annots = tree.asInstanceOf[MemberDef].mods.annotations
      annots foreach {
        case Apply(Select(New(tree), p), args) => val ap = Apply(tree, args)
          print("@", ap, " ")
        case ann => print("@" + ann + " ")
      }
    }

    override def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        print("["); printSeq(ts){ t =>
          printAnnotations(t)
          if (t.mods.hasFlag(CONTRAVARIANT)) {
            print("-")
          } else if (t.mods.hasFlag(COVARIANT)) {
            print("+")
          }
          printParam(t)
        }{print(", ")}; print("]")
      }
    }

    def backquotedPath(t: Tree): String = {
      t match {
        case Select(qual, name) if (name.isTermName & (qual match {
            case _: If | _: Match | _: Try | _: Annotated => true
            case _ => false
          }))  => "(%s).%s".format(backquotedPath(qual), symName(t, name))
        case Select(qual, name) if name.isTermName  => "%s.%s".format(backquotedPath(qual), symName(t, name))
        case Select(qual, name) if name.isTypeName  => "%s#%s".format(backquotedPath(qual), symName(t, name))
        case Ident(name)                            => symName(t, name)
        case _                                      => show(t, true)
      }
    }

    def getCurrentContext(): Tree = if (!contextStack.isEmpty) contextStack.top else null

    def removeDefaultTypesFromList(trees: List[Tree])(classesToRemove: List[String])(traitsToRemove: List[String]) =
      removeDefaultTraitsFromList(removeDefaultClassesFromList(trees, classesToRemove), traitsToRemove)

    def removeDefaultClassesFromList(trees: List[Tree], classesToRemove: List[String]) = trees filter {
      //TODO try without first line
      case Select(Ident(sc), name) => !((classesToRemove.contains(name.toString)) && (sc.toString == "scala"))
      //case Ident(name) => !classesToRemove.contains(name.toString)
      case _ => true
    }

    def removeDefaultTraitsFromList(trees: List[Tree], traitsToRemove: List[String]): List[Tree] =
      trees match {
        case Nil => trees
        case list : List[Tree] => list.last match {
          case Select(Ident(sc), name) if ((traitsToRemove.contains(name.toString)) && (sc.toString == "scala"))
            => removeDefaultTraitsFromList(list.init, traitsToRemove)
          case _ => list
         }
      }

    override def printTree(tree: Tree) {
      tree match {
        case EmptyTree =>

        //TODO - method to get primary constructor
        //TODO - method to get auxilary constructor
        //TODO - method to get defdef

        case ClassDef(mods, name, tparams, impl) =>
          contextStack.push(tree)
          printAnnotations(tree)
          printModifiers(tree, mods)
          val word =
            if (mods.isTrait) "trait"
            else if (ifSym(tree, _.isModuleClass)) "object"
            else "class"

          print(word, " ", symName(tree, name))

          printTypeParams(tparams)

          val Template(parents @ List(_*), self, methods) = impl
          //contextStack.push(tree)
          if (!mods.isTrait) {
            val templateVals = methods collect {
              case ValDef(mods, name, _, _) => (name, mods)
            }

            val primaryConstr = methods collectFirst {
              case dd: DefDef if dd.name == nme.CONSTRUCTOR => dd
            } getOrElse (null)

            val cstrMods = if (primaryConstr != null) primaryConstr.mods else null
            val ctparams = if (primaryConstr != null) primaryConstr.tparams else null
            val vparamss = if (primaryConstr != null) primaryConstr.vparamss else null
            val tp = if (primaryConstr != null) primaryConstr.tpt else null
            val rhs = if (primaryConstr != null) primaryConstr.rhs else null


            //TODO combine modifiers from vals and defs
            //TODO remove duplicate annotations
            val printParamss = if (primaryConstr != null) {
              vparamss map {
                vparams =>
                  if (vparams.isEmpty) vparams
                  else vparams map {
                    vparam =>
                    val templateVal = templateVals find {
                      _._1 == vparam.name
                    } getOrElse null
                    if (templateVal != null)
                      ValDef(Modifiers(vparam.mods.flags | templateVal._2.flags, templateVal._2.privateWithin, (vparam.mods.annotations ::: templateVal._2.annotations) distinct), vparam.name, vparam.tpt, vparam.rhs)
                      else vparam
                  }
              }
            } else null
//            val printParamss2 = if (primaryConstr != null) (vparamss(0), templateVals).zipped.map((x, y) =>
//              ValDef(Modifiers(x.mods.flags | y._2.flags, x.mods.privateWithin, (x.mods.annotations ::: y._2.annotations) distinct), x.name, x.tpt, x.rhs))
//            else null

            if (primaryConstr != null) {
              //constructor's modifier
              if (cstrMods.hasFlag(AccessFlags)) {
                print(" ")
                printModifiers(primaryConstr, cstrMods)
              } else print(" ")

              //constructor's params
              System.out.println("printParamss: " + printParamss)
              printParamss foreach { printParams =>
                //don't print single empty constructor param list
                if (!(printParams.isEmpty && printParamss.size == 1) || cstrMods.hasFlag(AccessFlags)) {
                  printConstrParams(printParams, true)
                  print(" ")
                }
              }
            } else print(" ")
          } else print(" ")

          //get trees without default classes and traits (when they are last)
          val printedParents = removeDefaultTypesFromList(parents)(List("AnyRef"))(if (mods.hasFlag(CASE)) List("Product", "Serializable") else Nil)
          //removeDefaultClassesFromList(parents, if (mods.hasFlag(CASE)) List("AnyRef", "Product", "Serializable") else List("AnyRef"))
          //TODO - current problem
          //if (name.toString().contains("TestLazy")) {
            System.out.println("=== In Class Def ===")
            System.out.println("name: " + name)
            System.out.println("showRaw tree: " + showRaw(tree))
            System.out.println("show tree (using global): " + global.show(tree))
            System.out.println("parents: " + parents + "\n")
            System.out.println("printedParents: " + printedParents + "\n")
            System.out.println("=========")
            //System.out.println("parentsWAnyRef: " + parentsWAnyRef)
          //}

          //pre-init block possible only if there are printed parents
          print(if (mods.isDeferred) "<: " else if (!printedParents.isEmpty) "extends "
            else "", impl)
          contextStack.pop()

        case PackageDef(packaged, stats) =>
          contextStack.push(tree)
          packaged match {
            case Ident(name) if name == nme.EMPTY_PACKAGE_NAME =>
              //printColumn(stats, " {", ";", "}")
              printSeq(stats) {
                print(_)
              } {
                print(";");
                println()
              };
            case _ =>
              printAnnotations(tree)
              print("package ", packaged);
              printColumn(stats, " {", ";", "}")
          }
          contextStack.pop()

        case ModuleDef(mods, name, impl) =>
          contextStack.push(tree)
          printAnnotations(tree)
          printModifiers(tree, mods);
          val Template(parents @ List(_*), self, methods) = impl
          val parentsWAnyRef = removeDefaultClassesFromList(parents, List("AnyRef"))
          //if (name.toString().contains("Scalaz")) {
            System.out.println("=== In Module Def ===")
            System.out.println("name: " + name)
            System.out.println("showRaw tree: " + showRaw(tree))
            System.out.println("show tree (using global): " + global.show(tree))
            System.out.println("parents: " + parents + "\n")
            System.out.println("parentsWAnyRef: " + parentsWAnyRef + "\n")
            System.out.println("=========")
            //System.out.println("parentsWAnyRef: " + parentsWAnyRef)
          //}
          //contextStack.push(tree)
          print("object " + symName(tree, name), if (!parentsWAnyRef.isEmpty) " extends " else " ", impl)
          contextStack.pop()

        case vd@ValDef(mods, name, tp, rhs) =>
          //if (name.toString() == ("x$11")) {
            //System.out.println("showRaw tree: " + showRaw(tree) + "\n")
            //System.out.println("show tree (using global): " + global.show(tree) + "\n")
          //}
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ", symName(tree, name))
          if (name.endsWith("_")) print(" "); printOpt(": ", tp)
          contextStack.push(tree)
          if (!mods.isDeferred)
            print(" = ", if (rhs.isEmpty) "_" else rhs)
          contextStack.pop()
          val a: (=> Int) => Int = null

        case dd@DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          //sym info isn't set after parser
          //if (name.toString().contains("mkCompoundTpt")) {
            //System.out.println("showRaw tree: " + showRaw(tree) + "\n")
            //System.out.println("show tree (using global): " + global.show(tree) + "\n")
          //}
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("def " + symName(tree, name))
          printTypeParams(tparams);
          vparamss foreach printValueParams
          if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty) && name.endsWith("_"))
            print(" ")
          printOpt(": ", tp);
          contextStack.push(tree)
          printOpt(" = " + (if (mods.hasFlag(MACRO)) "macro " else ""), rhs)
          contextStack.pop()

        case TypeDef(mods, name, tparams, rhs) =>
//          if (name.toString().contains("Disj")) {
//            System.out.println("showRaw tree: " + showRaw(tree) + "\n")
//            System.out.println("show tree (using global): " + global.show(tree) + "\n")
//          }
          if (mods hasFlag (PARAM | DEFERRED)) {
            printAnnotations(tree)
            printModifiers(tree, mods);
            print("type ");
            printParam(tree)
          } else {
            printAnnotations(tree)
            printModifiers(tree, mods);
            print("type " + symName(tree, name))
            printTypeParams(tparams);
            contextStack.push(tree)
            printOpt(" = ", rhs)
            contextStack.pop()
          }

        case LabelDef(name, params, rhs) =>
          if (name.contains("while$")) {
            contextStack.push(tree)
            val If(cond, thenp, elsep) = rhs
            print("while (", cond, ") ")

            val Block(list, wh) = thenp
            printColumn(list, "", ";", "")

            contextStack.pop()
          } else if (name.contains("doWhile$")) {
            contextStack.push(tree)
            val Block(bodyList: List[Tree], ifCond @ If(cond, thenp, elsep)) = rhs
            print("do ")
            printColumn(bodyList, "", ";", "")
            print(" while (", cond, ") ")
            //TODO - see match
            contextStack.pop()
          } else {
            print(symName(tree, name)); printLabelParams(params);
            contextStack.push(tree)
            printBlock(rhs)
            contextStack.pop()
          }

        case Import(expr, selectors) =>
          // Is this selector remapping a name (i.e, {name1 => name2})
          def isNotRemap(s: ImportSelector): Boolean = (s.name == nme.WILDCARD || s.name == s.rename)
          def selectorToString(s: ImportSelector): String = {
            val from = quotedName(s.name)
            if (isNotRemap(s)) from
            else from + "=>" + quotedName(s.rename)
          }
          print("import ", backquotedPath(expr), ".")
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) print(selectorToString(s))
              else print("{", selectorToString(s), "}")
            // If there is more than one selector braces are always needed
            case many =>
              print(many.map(selectorToString).mkString("{", ", ", "}"))
          }

        case Template(parents, self, body) =>
          System.out.println("in Template...")
          //TODO separate classes and templates
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
          //if (parents exists isReferenceToAnyVal) {
          //  print("AnyVal")
          //}
          //  else {

          val primaryCtr = body collectFirst {
            case dd: DefDef => dd
          } getOrElse (null)

          var ap: Apply = null
          var ctArgs: List[Tree] = null
          var presuperVals: List[Tree] = null


//          primaryCtr match {
//            case DefDef(_, _, _, _, _, Block(List(apply@Apply(_, crtArs)), _)) =>
//              ap = apply;
//              ctArgs = crtArs
//            case _ =>
//          }

        //TODO refactor it
          primaryCtr match {
            case DefDef(_, _, _, _, _, Block(ctBody @ List(_*), _)) =>
              ctBody collectFirst {
                  case apply@Apply(_, crtArs) =>
                    ap = apply
                    ctArgs = crtArs
              }
              presuperVals = ctBody filter {
                _ match {
                  case vd:ValDef => vd.mods.hasFlag(PRESUPER)
                  case _ => false
                }
              }
//              ap = apply;
//              ctArgs = crtArs
            case _ =>
          }

          System.out.println("ap: " + ap)
          System.out.println("ctArgs: " + ctArgs)
          System.out.println("presuperVals: " + presuperVals)

          val printedParents = //if (!getCurrentContext().isInstanceOf[TypeDef]) removeTypeFromList(parents) else parents
            getCurrentContext() match {
              //val example: Option[AnyRef => Product1[Any] with AnyRef] = ... - CompoundTypeTree with template
              case _: CompoundTypeTree => parents
              case ClassDef(mods, name, _, _) if mods.hasFlag(CASE) => removeDefaultTypesFromList(parents)(List("AnyRef"))(List("Product", "Serializable"))
              case _ => removeDefaultClassesFromList(parents, List("AnyRef"))
            }

          //pre-init block representation
          if (presuperVals != null && !presuperVals.isEmpty) {
            print("{")
            printColumn(presuperVals, "", ";", "")
            print("} " + (if (!printedParents.isEmpty) "with " else ""))
          }
          //if pre-init params are presented
          //print {
          //print vals
          //print } with

          if (!printedParents.isEmpty) {
            val (clParent :: traits) = printedParents
            print(clParent)

            //TODO remove all tree.filter (reimplement using Traversers)
            def getConstrParams(tree: Tree, cargs: List[List[Tree]]): List[List[Tree]] = {
              tree match {
                case Apply(inTree, args) =>
                  getConstrParams(inTree, cargs):+args //not very good decision
                case _ => cargs
              }
            }
            val applyParamsList = getConstrParams(ap, Nil)

            //pass parameters to super class constructors
            //get all parameter lists
            //this implementation is not correct
            //val applyParamsList = if (ctArgs != null && !ctArgs.isEmpty){
                //ap filter {
                  //case apply @ Apply(_, args) => true
                  //case _ => false
                //} map {
                  //apply => val Apply(sel, ars) = apply
                  //ars
                //} reverse
              //} else List(List())
            applyParamsList foreach {x: List[Tree] => if (!(x.isEmpty && applyParamsList.size == 1)) printRow(x, "(", ", ", ")")}

            if (!traits.isEmpty) {
              printRow(traits, " with ", " with ", "")
            }
          }
          //remove primary constr def and constr val and var defs
          //right contains all constructors
          //TODO understand how filter works
          val (left, right) = body.filter {
            //remove valdefs defined in constructor and pre-init block
            case vd: ValDef => !vd.mods.hasFlag(PARAMACCESSOR) && !vd.mods.hasFlag(PRESUPER)
            case dd: DefDef => dd.name != nme.MIXIN_CONSTRUCTOR //remove $this$ from traits
            case EmptyTree => false
            case _ => true
          } span {
            case dd: DefDef => dd.name != nme.CONSTRUCTOR
            case _ => true
          }

          val modBody = left ::: right.drop(1)//List().drop(1) ==> List()

          if (!modBody.isEmpty || !self.isEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name);
              printOpt(": ", self.tpt);
              print(" => ")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : ", self.tpt, " => ")
            } else {
              print(" {")
            }
            contextStack.push(tree)
            printColumn(modBody, "", ";", "}")
            contextStack.pop()
          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          contextStack.push(tree)
          printColumn(stats ::: List(expr), "{", ";", "}")
          contextStack.pop()

        case Match(selector, cases) =>
//          def insertBraces(body: =>Unit) {
//            selector match {
//              case m: Match =>
//                print("(")
//                body
//                print(")")
//              case _ => body
//            }
//          }

          //insert braces if match is inner
          //make this function available for other casses
          //passing required type for checking
          def insertBraces(body: =>Unit) {
            if (contextStack.exists{
              _.isInstanceOf[Match]
            }) {
                print("(")
                body
                print(")")
            } else body
          }

          val selectorType1 = selectorType
          selectorType = selector.tpe

          val printParanthesis = selector match {
            case _: If | _: Match | _: Try | _: Annotated => true
            case _ => false
          }

          tree match {
            case Match(EmptyTree, cs) =>
              printColumn(cases, "{", "", "}")
            case _ =>
              insertBraces {
                contextStack.push(tree)
                if (printParanthesis) print("(")
                print(selector);
                if (printParanthesis) print(")")
                contextStack.pop()

                printColumn(cases, " match {", "", "}")
              }
          }

          selectorType = selectorType1

        case CaseDef(pat, guard, body) =>
          print("case ")
          def patConstr(pat: Tree): Tree = pat match {
            case Apply(fn, args) => patConstr(fn)
            case _ => pat
          }
          if (showOuterTests &&
            needsOuterTest(
              patConstr(pat).tpe.finalResultType, selectorType, currentOwner))
            print("???")
          print(pat);
          printOpt(" if ", guard)
          contextStack.push(tree)
          print(" => ", body)
          contextStack.pop()

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          //print("(", elem, ")*")
          print(elem, "*")

        case Bind(name, t) =>
          //Bind(tpnme.WILDCARD, EmptyTree)
          if (t == EmptyTree) print("(", symName(tree, name), ")")
          else if (t.exists{
            case _:Star => true
            case _ => false
          }) print(symName(tree, name), " @ ", t)
          else print("(", symName(tree, name), " @ ", t, ")")
          //TODO try this variant
          //else print(symName(tree, name), " @ ", t)

        case UnApply(fun, args) =>
          print(fun, " <unapply> "); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print("Array[", elemtpt); printRow(trees, "]{", ", ", "}")

        case Function(vparams, body) =>
          print("(");
          printValueParams(vparams, true);
          print(" => ", body, ")")
          if (printIds && tree.symbol != null) print("#" + tree.symbol.id)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case AssignOrNamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ")");
          indent;
          println()
          print(thenp);
          undent
          if (!elsep.isEmpty) {
            println();
            print("else");
            indent;
            println();
            print(elsep);
            undent
          }

        case Return(expr) =>
          print("return ", expr)

        case Try(block, catches, finalizer) =>
          print("try ");
          printBlock(block)
          if (!catches.isEmpty) printColumn(catches, " catch {", "", "}")
          printOpt(" finally ", finalizer)

        case Throw(expr) =>
          print("throw ", expr)

        case New(tpe) =>
          print("new ", tpe)

        case Typed(expr, tp) =>
          tp match {
            case Function(List(), EmptyTree) => print("(", expr, " _)") //func _
            case _ => print("((", expr, "): ", tp, ")") //parenteses required when (a match {}) : Type
          }

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]")

        case Apply(fun, vargs) =>
          //processing methods ended on colon with multiple args list//
          //TODO see processing of methods with multiple arguments lists and named *:
          //example:
//          def t[A,B](as: List[A]) = {
//            println("hello")
//            ((Map.empty[B, List[A]]) /: as){ (nels, a) => println(""); (nels)}
//          }
//         by default results in:
//        {
//          val x$1 = Map.empty[B, List[A]];
//          as.$div$colon(x$1)
//        }(((nels, a) => {
//          println("");
//          nels
//        }))
          tree match {
            //processing of methods started with colons x :\ list
            case Apply(Block(l1 @ List(sVD :ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))), l3 @ List(_*))
              if sVD.mods.hasFlag(SYNTHETIC) && methodName.toString.endsWith("$colon") && (sVD.name == iVDName) =>
                val printBlock = Block(l1, Apply(a1, l3))
                print(printBlock)
            //TODO find more general way to include matches, ...
            case Apply(_: If, _) | Apply(_: Try, _) | Apply(_: Match, _) | Apply(_: LabelDef, _) => print("(", fun, ")"); printRow(vargs, "(", ", ", ")")
            case _ => print(fun); printRow(vargs, "(", ", ", ")")
          }

          //TODO - find cases
        case ApplyDynamic(qual, vargs) =>
          print("<apply-dynamic>(", qual, "#", tree.symbol.nameString)
          printRow(vargs, ", (", ", ", "))")

        case Super(This(qual), mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case Super(qual, mix) =>
          print(qual, ".super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case This(qual) =>
          if (!qual.isEmpty) print(symName(tree, qual) + ".")
          print("this")

        //case Select(apply: Apply, name) if (!settings.debug.value) =>
        //print(apply,".",symName(tree, name))

        case Select(qual@New(tpe), name) if (!settings.debug.value) =>
          print(qual)

        case Select(qualifier, name) =>
          qualifier match {
            case _: Match | _: If | _: Try | _: LabelDef => print("(", backquotedPath(qualifier), ").", symName(tree, name))
            case _ => print(backquotedPath(qualifier), ".", symName(tree, name))
          }

        case id@Ident(name) =>
          if (!name.isEmpty) {
            val str = symName(tree, name)
            print(if (id.isBackquoted) "`" + str + "`" else str)
          }
          else {
            print("")
          }

        case Literal(x) =>
          //processing Float constants
          val printValue = x.escapedStringValue + (if (x.value.isInstanceOf[Float]) "F" else "") //correct printing of Float
          print(printValue)

        case tt: TypeTree =>
          if ((tree.tpe eq null) || (doPrintPositions && tt.original != null)) {
            if (tt.original != null) print("<type: ", tt.original, ">")
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString)
          } else {
            print(tree.tpe.toString)
          }

        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@", tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
          }

        //TODO combine all similar methods
          val printParanthesis = tree match {
            //TODO check - do we need them for label defs (while, do while)
            case _: If | _: Match| _: Try | _: Annotated | _: LabelDef => true
            case _ => false
          }

//          val unchecked = tpt match {
//              //TODO rewrite
//            case Select(_, tname) => tname.toString() == "unchecked"
//            //c: @unchecked match {
//            case tname => tname.toString() == "unchecked"
//            //case _ => false
//          }
//          print(tree, if (tree.isType) " " else if (!unchecked) ": " else "")
//          if (!unchecked) printAnnot()
        print(if (printParanthesis) "(" else "",tree, if (printParanthesis) ")" else "",if (tree.isType) " " else ": ")
        printAnnot()


        case SingletonTypeTree(ref) =>
          print(ref, ".type")

        //type $u2200[P[_]] = $u00AC[$u2203[scala.AnyRef {
          //type λ[X] = $u00AC[P[X]]
        //}#λ]]
        //TODO change and see the three if tree.tree
        case SelectFromTypeTree(qualifier, selector) =>
          print("(", qualifier, ")#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          contextStack.push(tree)
          print(templ)
          contextStack.pop()

        case AppliedTypeTree(tp, args) =>
          //TODO find function types
          //get name of base type
          //val functions = 0 to 27 map { "Function" + _ }
          //if (functions.contains(nameOfType) ...
          //for name => ...
          //TODO (TOASK) find stadart solution - now it's possuble that another Function redefined

          //TODO move it to class declarations
          val functionName = "Function"
          val funcTypeNames = 0 to 22 map { "Function" + _ }
          //System.out.println("funcTypeNames: " + funcTypeNames)

          def isFunctionType =
            tp match {
              //_root_.scala.Function0[String]
              case Select(qual, name) => funcTypeNames.contains(name.toString)
              //Function0[String]
              case Ident(name) => funcTypeNames.contains(name.toString)
              case _ => false
            }

          if (isFunctionType) {
            print("(")
            printRow(args.init, "(", ", ", ")")
            print(" => ", args.last, ")")
          } else {
            val typ = tree.tpe
            //System.out.println("typ: " + typ)
            val sym = tree.symbol
            //System.out.println("sym: " + sym)
            //processing of repeated type
            if (tp.exists {
              case Select(_, name) => name == tpnme.REPEATED_PARAM_CLASS_NAME
              case _ => false
            } && !args.isEmpty) {
              print(args(0), "*")
            } else if (tp match {
              case Select(_, name) => name == tpnme.BYNAME_PARAM_CLASS_NAME
              case _ => false
            }) {
              print("=> ", if (args.isEmpty) "()" else args(0))
            } else {
              print(tp);
              printRow(args, "[", ", ", "]")
            }
          }

        case TypeBoundsTree(lo, hi) =>
          printOpt(" >: ", lo); printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print("(", tpt);
          printColumn(whereClauses, " forSome { ", ";", "})")

        // SelectFromArray is no longer visible in reflect.internal.
        // eliminated until we figure out what we will do with both Printers and
        // SelectFromArray.
        //          case SelectFromArray(qualifier, name, _) =>
        //          print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          xprintTree(this, tree)
      }
      if (printTypes && tree.isTerm && !tree.isEmpty) {
        print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
      val sym = tree.symbol
      if (sym.name.toString == nme.ERROR.toString) {
        "<" + quotedName(name, decoded) + ": error>"
      } else if (sym != null && sym != NoSymbol) {
        val prefix = if (sym.isMixinConstructor) "/*%s*/".format(quotedName(sym.owner.name, decoded)) else ""
        var suffix = ""
        if (settings.uniqid.value) suffix += ("#" + sym.id)
        //if (settings.Yshowsymkinds.value) suffix += ("#" + sym.abbreviatedKindString)
        prefix + quotedName(tree.symbol.decodedName) + suffix
      } else {
        //val str = if (nme.isConstructorName(name)) "this"
        val str = if (name == nme.CONSTRUCTOR) "this"
        else quotedName(name, decoded)
        str
      }
    }

    def decodedSymName(tree: Tree, name: Name) = symNameInternal(tree, name, true)

    def symName(tree: Tree, name: Name) = symNameInternal(tree, name, false)


    override def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printPosition(tree)
        printTree(
          if (tree.isDef && tree.symbol != NoSymbol && tree.symbol.isInitialized) {
            tree match {
              case ClassDef(_, _, _, impl@Template(ps, emptyValDef, body))
                if (tree.symbol.thisSym != tree.symbol) =>
                ClassDef(tree.symbol, Template(ps, ValDef(tree.symbol.thisSym), body))
              case ClassDef(_, _, _, impl) => ClassDef(tree.symbol, impl)
              case ModuleDef(_, _, impl) => ModuleDef(tree.symbol, impl)
              case ValDef(_, _, _, rhs) => ValDef(tree.symbol, rhs)
              case DefDef(_, _, _, vparamss, _, rhs) => DefDef(tree.symbol, vparamss, rhs)
              case TypeDef(_, _, _, rhs) => TypeDef(tree.symbol, rhs)
              case _ => tree
            }
          } else tree)
      case unit: CompilationUnit =>
        print("// Scala source: " + unit.source + "\n")
        if (unit.body == null) print("<null>")
        else {
          print(unit.body);
          println()
        }
        println()
        out.flush()
      case name: Name =>
        print(quotedName(name))
      case arg =>
        out.print(if (arg == null) "null" else arg.toString)
    }

    //print from internal
    //  override def print(args: Any*): Unit = args foreach {
    //    case tree: Tree =>
    //      printPosition(tree)
    //      printTree(tree)
    //    case name: Name =>
    //      print(quotedName(name))
    //    case arg =>
    //      out.print(if (arg == null) "null" else arg.toString)
    //  }

  }

}
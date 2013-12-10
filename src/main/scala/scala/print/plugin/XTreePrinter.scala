package scala.print.plugin

import java.io.{StringWriter, PrintWriter}
import scala.reflect.internal.Flags._
import scala.reflect.internal._
import scala.reflect.internal.util.{SourceFile, Statistics}
import scala.collection.mutable

object XTreePrinter{
  def apply(compiler: Reflect) = {
    new XTreePrinter {
      val global = compiler
    }
  }
  
  type Reflect = SymbolTable
}

trait XTreePrinter {

  import XTreePrinter.Reflect
  val global: Reflect
  import global._

  private var prettyPrinter: PrettyPrinter = null
  
  def printer: global.TreePrinter = prettyPrinter
  
  //TODO set printMultiline for false
  def show(what: Reflect#Tree, subPrinter: global.TreePrinter, printMultiline: Boolean = false, decodeNames: Boolean = false) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    prettyPrinter = new PrettyPrinter(writer, subPrinter)

    prettyPrinter.print(what)
    writer.flush()
    buffer.toString
  }

  val printMultiline: Boolean = false
  val decodeNames: Boolean = true
  
  protected def compareNames(name1: Reflect#Name, name2: Reflect#Name) =
    !Option(name1).isEmpty && !Option(name2).isEmpty && (name1.toString.trim == name2.toString.trim)

  //TODO change printMultiline (introduce class for settings) - to pass all parameters
  class PrettyPrinter(out: PrintWriter, subPrinter: global.TreePrinter) extends global.TreePrinter(out) {
    
    //TODO maybe we need to pass this stack when explicitly run show inside print
    val contextStack = scala.collection.mutable.Stack[Tree]()

    override def printModifiers(tree: Tree, mods: Modifiers): Unit = printModifiers(tree, mods, false)

    def printModifiers(tree: Tree, mods: Modifiers, isCtr: Boolean): Unit =
      if (getCurrentContext().isEmpty || modsAccepted)
        printFlags(mods.flags, "" + mods.privateWithin, isCtr)
      else
        List(IMPLICIT, CASE, LAZY).foreach{flag => if(mods.hasFlag(flag))  printFlags(flag, "", isCtr)}

    def modsAccepted = getCurrentContext() map {
      case _:ClassDef | _:ModuleDef | _:Template | _:PackageDef => true
      case _ => false
    } getOrElse false

    override def printFlags(flags: Long, privateWithin: String) =
      printFlags(flags, privateWithin, false)

    def printFlags(flags: Long, privateWithin: String, isCtr: Boolean) {
      val base = PROTECTED | OVERRIDE | PRIVATE | ABSTRACT | FINAL | SEALED | LAZY | LOCAL
      val mask = if (isCtr) base else base | IMPLICIT

      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") subPrinter.print(s + " ")
      //case flag should be the last
      val caseFlag = flagsToString(flags & CASE)
      if (!caseFlag.isEmpty) subPrinter.print(caseFlag + " ")
      //abs override flag should be the last
      val absOverrideFlag = flagsToString(flags & ABSOVERRIDE)
      if (!absOverrideFlag.isEmpty) subPrinter.print("abstract override ")
    }

    def printConstrParams(ts: List[ValDef], isConstr: Boolean) {
      codeInParantheses(){
        if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
        printSeq(ts) {
          printParam(_, true)
        } { subPrinter.print(", ") }
      }
    }

    override def printValueParams(ts: List[ValDef]) {
      printValueParams(ts, false)
    }

    def printValueParams(ts: List[ValDef], isFuncTree: Boolean) {
      //val a: Int => Int = implicit x => x //parantheses are not allowed here
      val printParanthesis = !isFuncTree || {
        ts match {
          case List(vd: ValDef) => !vd.mods.hasFlag(IMPLICIT)
          case _ => true
        }
      }

      if (printParanthesis)
        super.printValueParams(ts)
      else {
        if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
        printSeq(ts) {
          printParam
        } { subPrinter.print(", ") }
      }
    }

    def printParam(tree: Tree, isConstr: Boolean) {
      tree match {
        case ValDef(mods, name, tp, rhs) =>
//          printPosition(tree)
          printAnnotations(tree)
          if (isConstr) {
            printModifiers(tree, mods, isConstr)
          }
          subPrinter.print(if (mods.isMutable && isConstr) "var " else if (isConstr) "val " else "", symbName(tree, name));
          if (name.endsWith("_")) subPrinter.print(" ");
          printOpt(": ", tp);
          printOpt(" = ", rhs)
        case _ => super.printParam(tree)
      }
    }

    override def printParam(tree: Tree) {
      printParam(tree, false)
    }

    override def printAnnotations(tree: Tree) {
      val annots = tree.asInstanceOf[MemberDef].mods.annotations
      annots foreach {
        case Apply(Select(New(tree), p), args) => val ap = Apply(tree, args)
          subPrinter.print("@", ap, " ")
        case ann => subPrinter.print("@" + ann + " ")
      }
    }

    override def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        subPrinter.print("["); printSeq(ts){ t =>
          printAnnotations(t)
          if (t.mods.hasFlag(CONTRAVARIANT)) {
            subPrinter.print("-")
          } else if (t.mods.hasFlag(COVARIANT)) {
            subPrinter.print("+")
          }
          printParam(t)
        }{subPrinter.print(", ")}; subPrinter.print("]")
      }
    }

    def codeInParantheses(condition: Boolean = true)(body: =>Unit) {
      if (condition) subPrinter.print("(")
      body
      if (condition) subPrinter.print(")")
    }

    def specialTreeContext(context: Tree)(iIf: Boolean = true, iMatch: Boolean = true,
        iTry: Boolean = true, iAnnotated: Boolean = true, iBlock: Boolean = true, iLabelDef: Boolean = true) = {
      context match {
        case _: If => iIf
        case _: Match => iMatch
        case _: Try => iTry
        case _: Annotated => iAnnotated
        case _: Block => iBlock
        case _: LabelDef => iLabelDef
        case _ => false
      }
    }

    protected def isIntLitWithDecodedOp(qual: Tree, name: Name) = {
      lazy val qualIsIntLit = qual match {
        case Literal(x) => x.value.isInstanceOf[Int]
        case _ => false
      }
      decodeNames && qualIsIntLit && name.isOperatorName
    }

    //Danger while using inheritance: it's hidden (overwritten) method
    def backquotedPath(t: Tree): String = {
      t match {
        case Select(qual, name) if (name.isTermName && specialTreeContext(qual)(iLabelDef = false)) || isIntLitWithDecodedOp(qual, name) => "(%s).%s".format(backquotedPath(qual), symbName(t, name))
        case Select(qual, name) if name.isTermName  => "%s.%s".format(backquotedPath(qual), symbName(t, name))
        case Select(qual, name) if name.isTypeName  => "%s#%s".format(backquotedPath(qual), symbName(t, name))
        case Ident(name)                            => symbName(t, name)
        case _                                      => show(t, subPrinter, printMultiline, decodeNames)
      }
    }

    def contextManaged(context: Tree)(body: =>Unit) {
      contextStack.push(context)
      body
      contextStack.pop()
    }

    def getCurrentContext() = if (!contextStack.isEmpty) Some(contextStack.top) else None

    def removeDefaultTypesFromList(trees: List[Tree])(classesToRemove: List[String])(traitsToRemove: List[String]) =
      removeDefaultTraitsFromList(removeDefaultClassesFromList(trees, classesToRemove), traitsToRemove)

    def removeDefaultClassesFromList(trees: List[Tree], classesToRemove: List[String]) = trees filter {
      case Select(Ident(sc), name) => !((classesToRemove.contains(name.toString)) && (sc.toString == "scala"))
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

    def getPrimaryConstr(methods: List[Tree]) =
      methods collectFirst {
        case dd: DefDef if dd.name.toString.trim == nme.CONSTRUCTOR.toString.trim => dd
      }

    // Is this selector remapping a name (i.e, {name1 => name2})
    protected def isNotRemap(s: ImportSelector): Boolean =
      (compareNames(s.name, nme.WILDCARD) || compareNames(s.name, s.rename))

    protected def selectorToString(s: ImportSelector): String = {
      val from = quotedName(s.name)
      if (isNotRemap(s)) from
      else from + "=>" + quotedName(s.rename)
    }

    override def printTree(tree: Tree) {
      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          contextManaged(tree){
            printAnnotations(tree)
            val word =
              if (mods.isTrait){
                printModifiers(tree, mods &~ ABSTRACT) // avoid abstract modifier for traits
                "trait"
              } else {
                printModifiers(tree, mods)
                "class"
              }

            subPrinter.print(word, " ", symbName(tree, name))
            printTypeParams(tparams)

            val Template(parents @ List(_*), self, methods) = impl
            if (!mods.isTrait) {
              val templateVals = methods collect {
                case ValDef(mods, name, _, _) => (name, mods)
              }

              val primaryConstrOpt = getPrimaryConstr(methods)

              primaryConstrOpt map {
                primaryConstr =>

                val cstrMods = primaryConstr.mods
                val vparamss = primaryConstr.vparamss

                //combine modifiers
                val printParamss =
                  vparamss map {
                    vparams =>
                      if (vparams.isEmpty) vparams
                      else vparams map {
                        vparam =>
                          templateVals find {
                            tv =>
                              compareNames(tv._1, vparam.name)
                          } map {
                            templateVal =>
                              ValDef(Modifiers(vparam.mods.flags | templateVal._2.flags, templateVal._2.privateWithin,
                                (vparam.mods.annotations ::: templateVal._2.annotations) distinct), vparam.name, vparam.tpt, vparam.rhs)
                          } getOrElse vparam
                      }
                    }

                //constructor's modifier
                if (cstrMods.hasFlag(AccessFlags)) {
                  subPrinter.print(" ")
                  printModifiers(primaryConstr, cstrMods)
                }

                //constructor's params
                printParamss foreach { printParams =>
                  //don't print single empty constructor param list
                  if (!(printParams.isEmpty && printParamss.size == 1) || cstrMods.hasFlag(AccessFlags)) {
                    printConstrParams(printParams, true)
                    subPrinter.print(" ")
                  }
                }
              } getOrElse {subPrinter.print(" ")}
            }

            //get trees without default classes and traits (when they are last)
            val printedParents = removeDefaultTypesFromList(parents)(List("AnyRef"))(if (mods.hasFlag(CASE)) List("Product", "Serializable") else Nil)

            subPrinter.print(if (mods.isDeferred) "<: " else if (!printedParents.isEmpty) " extends "
              else "", impl)
          }

        case PackageDef(packaged, stats) =>
          contextManaged(tree){
            packaged match {
              case Ident(name) if compareNames(name, nme.EMPTY_PACKAGE_NAME) =>
                printSeq(stats) {
                  subPrinter.print(_)
                } {
                  subPrinter.print(";");
                  println()
                };
              case _ =>
                printAnnotations(tree)
                subPrinter.print("/*after-namer-package:*/ package ", packaged);
                printColumn(stats, " {", "\n", "}")
            }
          }

        case ModuleDef(mods, name, impl) =>
          contextManaged(tree){
            printAnnotations(tree)
            printModifiers(tree, mods);
            val Template(parents @ List(_*), self, methods) = impl
            val parentsWAnyRef = removeDefaultClassesFromList(parents, List("AnyRef"))
            subPrinter.print("object " + symbName(tree, name), if (!parentsWAnyRef.isEmpty) " extends " else "", impl)
          }

        case vd@ValDef(mods, name, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          subPrinter.print(if (mods.isMutable) "var " else "val ", symbName(tree, name))
          if (name.endsWith("_")) subPrinter.print(" ")

          printOpt(
            // place space after symbolic def name (val *: Unit does not compile)
            (if(symbName(tree, name) != symbName(tree, name,false) || symbName(tree, name) != symbName(tree, name, true))
              " "
            else
              "") +
            ": ",
            tp
          )
          contextManaged(tree){
            if (!mods.isDeferred)
              subPrinter.print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd@DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          subPrinter.print("def " + symbName(tree, name))
          printTypeParams(tparams);
          vparamss foreach printValueParams
          if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty) && name.endsWith("_"))
            subPrinter.print(" ")
          printOpt(
            // place space after symbolic def name (def *: Unit does not compile)
            (if(symbName(tree, name) != symbName(tree, name,false) || symbName(tree, name) != symbName(tree, name, true))
              " "
            else
              "") +
            ": ",
            tp
          )
          contextManaged(tree){
            printOpt(" = " + (if (mods.hasFlag(MACRO)) "macro " else ""), rhs)
          }

        case td@TypeDef(mods, name, tparams, rhs) =>
          if (mods hasFlag (PARAM | DEFERRED)) {
            printAnnotations(tree)
            printModifiers(tree, mods);
            subPrinter.print("type ");
            printParam(tree)
          } else {
            printAnnotations(tree)
            printModifiers(tree, mods);
            subPrinter.print("type " + symbName(tree, name))
            printTypeParams(tparams);
            contextManaged(tree){
              printOpt(" = ", rhs)
            }
          }

        case LabelDef(name, params, rhs) =>
          if (name.contains("while$")) {
            contextManaged(tree){
              val If(cond, thenp, elsep) = rhs
              subPrinter.print("while (", cond, ") ")
              val Block(list, wh) = thenp
              printColumn(list, "", ";", "")
            }
          } else if (name.contains("doWhile$")) {
            contextManaged(tree){
              val Block(bodyList: List[Tree], ifCond @ If(cond, thenp, elsep)) = rhs
              subPrinter.print("do ")
              printColumn(bodyList, "", ";", "")
              subPrinter.print(" while (", cond, ") ")
            }
          } else {
            subPrinter.print(symbName(tree, name)); printLabelParams(params);
            contextManaged(tree){
              printBlock(rhs)
            }
          }

        case Import(expr, selectors) =>
          subPrinter.print("import ", backquotedPath(expr), ".")
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) subPrinter.print(selectorToString(s))
              else subPrinter.print("{", selectorToString(s), "}")
            // If there is more than one selector braces are always needed
            case many =>
              subPrinter.print(many.map(selectorToString).mkString("{", ", ", "}"))
          }

        case Template(parents, self, body) =>

          val printedParents =
            getCurrentContext() map {
              //val example: Option[AnyRef => Product1[Any] with AnyRef] = ... - CompoundTypeTree with template
              case _: CompoundTypeTree => parents
              case ClassDef(mods, name, _, _) if mods.hasFlag(CASE) => removeDefaultTypesFromList(parents)(List("AnyRef"))(List("Product", "Serializable"))
              case _ => removeDefaultClassesFromList(parents, List("AnyRef"))
            } getOrElse(parents)

          val primaryCtrOpt = getPrimaryConstr(body)
          var ap: Option[Apply] = None

          for (primaryCtr <- primaryCtrOpt) {
            primaryCtr match {
              case DefDef(_, _, _, _, _, Block(ctBody @ List(_*), _)) =>
                ap = ctBody collectFirst {
                  case apply: Apply => apply
                }

                //vals in preinit blocks
                val presuperVals = ctBody filter {
                  case vd:ValDef => vd.mods.hasFlag(PRESUPER)
                  case _ => false
                }

                if (!presuperVals.isEmpty) {
                  subPrinter.print("{")
                  printColumn(presuperVals, "", ";", "")
                  subPrinter.print("} " + (if (!printedParents.isEmpty) "with " else ""))
                }

              case _ =>
            }
          }

          if (!printedParents.isEmpty) {
            val (clParent :: traits) = printedParents
            subPrinter.print(clParent)

            def getConstrParams(tree: Tree, cargs: List[List[Tree]]): List[List[Tree]] = {
              tree match {
                case Apply(inTree, args) =>
                  getConstrParams(inTree, cargs):+args
                case _ => cargs
              }
            }

            val applyParamsList = ap map {getConstrParams(_, Nil)} getOrElse Nil
            applyParamsList foreach {x: List[Tree] => if (!(x.isEmpty && applyParamsList.size == 1)) printRow(x, "(", ", ", ")")}

            if (!traits.isEmpty) {
              printRow(traits, " with ", " with ", "")
            }
          }
          //remove primary constr def and constr val and var defs
          //right contains all constructors
          //TODO see impl filter on Tree
          val (left, right) = body.filter {
            //remove valdefs defined in constructor and pre-init block
            case vd: ValDef => !vd.mods.hasFlag(PARAMACCESSOR) && !vd.mods.hasFlag(PRESUPER)
            case dd: DefDef => !compareNames(dd.name, nme.MIXIN_CONSTRUCTOR) //remove $this$ from traits
            case EmptyTree => false
            case _ => true
          } span {
            case dd: DefDef => !compareNames(dd.name, nme.CONSTRUCTOR)
            case _ => true
          }

          val modBody = left ::: right.drop(1)//List().drop(1) ==> List()
          val showBody = !(modBody.isEmpty &&
            (self match {
              case ValDef(mods, name, TypeTree(), rhs) if (mods & PRIVATE) != 0 && name.decoded == "_" && rhs.isEmpty => true // workaround for superfluous ValDef when parsing class without body using quasi quotes
              case _ => self.isEmpty
            }))
          if (showBody) {
            if (!compareNames(self.name, nme.WILDCARD)) {
              subPrinter.print(" { ", self.name);
              printOpt(": ", self.tpt);
              subPrinter.print(" =>")
            } else if (!self.tpt.isEmpty) {
              subPrinter.print(" { _ : ", self.tpt, " =>")
            } else {
              subPrinter.print(" {")
            }
            contextManaged(tree) {
              printColumn(modBody, "", ";", "}")
            }
          }

        case Block(stats, expr) =>
          contextManaged(tree){
            printColumn(stats ::: List(expr), "{", ";", "}")
          }

        case Match(selector, cases) =>
          //insert braces if match is inner
          //make this function available for other casses
          //passing required type for checking
          def insertBraces(body: =>Unit) {
            if (contextStack.exists{
              _.isInstanceOf[Match]
            }) {
                subPrinter.print("(")
                body
                subPrinter.print(")")
            } else body
          }

          val printParantheses = specialTreeContext(selector)(iLabelDef = false)
          tree match {
            case Match(EmptyTree, cs) =>
              printColumn(cases, "{", "", "}")
            case _ =>
              insertBraces {
                contextManaged(tree){
                  codeInParantheses(printParantheses) {
                    subPrinter.print(selector);
                  }
                }
                printColumn(cases, " match {", "", "}")
              }
          }

        case CaseDef(pat, guard, body) =>
          subPrinter.print("case ")
          def patConstr(pat: Tree): Tree = pat match {
            case Apply(fn, args) => patConstr(fn)
            case _ => pat
          }

          subPrinter.print(pat);
          printOpt(" if ", guard)
          contextManaged(tree) {
            subPrinter.print(" => ", body)
          }

        case Star(elem) =>
          subPrinter.print(elem, "*")

        case Bind(name, t) =>
          if (t == EmptyTree) subPrinter.print("(", symbName(tree, name), ")")
          else if (t.exists{
            case _:Star => true
            case _ => false
          }) subPrinter.print(symbName(tree, name), " @ ", t)
          else subPrinter.print("(", symbName(tree, name), " @ ", t, ")")

        //almost the same as in original
        case Function(vparams, body) =>
          subPrinter.print("(");
          printValueParams(vparams, true);
          subPrinter.print(" => ", body, ")")

        case Typed(expr, tp) =>
          tp match {
            case Function(List(), EmptyTree) => subPrinter.print("(", expr, " _)") //func _
            case _ => subPrinter.print("((", expr, "): ", tp, ")") //parenteses required when (a match {}) : Type
          }

        case Apply(fun, vargs) =>
          //process methods ending on colon with multiple args list//
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
            //processing methods ending on colons (x \: list)
            case Apply(Block(l1 @ List(sVD :ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))), l3 @ List(_*))
              if sVD.mods.hasFlag(SYNTHETIC) && methodName.toString.endsWith("$colon") && compareNames(sVD.name, iVDName) => //&& (sVD.name.toString.trim == iVDName.toString.trim) =>
              val printBlock = Block(l1, Apply(a1, l3))
              subPrinter.print(printBlock)
            case Apply(tree1, _) if (specialTreeContext(tree1)(iAnnotated = false)) => codeInParantheses(){subPrinter.print(fun)}; printRow(vargs, "(", ", ", ")")
            case _ => subPrinter.print(fun); printRow(vargs, "(", ", ", ")")
          }


        case Super(This(qual), mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) subPrinter.print(symbName(tree, qual) + ".")
          subPrinter.print("super")
          if (!mix.isEmpty)
            subPrinter.print("[" + mix + "]")

        case This(qual) =>
          //symName is redefined
          if (!qual.isEmpty) subPrinter.print(symbName(tree, qual) + ".")
          subPrinter.print("this")

        //case Select(apply: Apply, name) if (!settings.debug.value) =>
        //subPrinter.print(apply,".",symName(tree, name))

        case Select(qual@New(tpe), name) =>
          subPrinter.print(qual)

        case Select(qualifier, name) => {
          val printParantheses = specialTreeContext(qualifier)(iAnnotated = false) || isIntLitWithDecodedOp(qualifier, name)
          if (printParantheses) subPrinter.print("(", backquotedPath(qualifier), ").", symbName(tree, name))
          else subPrinter.print(backquotedPath(qualifier), ".", symbName(tree, name))
        }

        case id@Ident(name) =>
          if (!name.isEmpty) {
            val str = symbName(tree, name)

            val strIsBackquoted = str.startsWith("`") && str.endsWith("`")

            subPrinter.print(if (id.isBackquoted && !strIsBackquoted) "`" + str + "`" else str)
          }
          else {
            subPrinter.print("")
          }

        case l@Literal(x) =>
          //TODO refactor multiline string processing - x.stringValue
          if (x.value.isInstanceOf[String] && printMultiline && x.stringValue.contains("\n") && !x.stringValue.contains("\"\"\"") && x.stringValue.size > 1) {
            val splitValue = x.stringValue.split('\n'.toString).toList
            val multilineStringValue = if (x.stringValue.endsWith("\n")) splitValue :+ "" else splitValue
            val trQuotes = "\"\"\""
            subPrinter.print(trQuotes); printSeq(multilineStringValue){subPrinter.print(_)}{subPrinter.print("\n")}; subPrinter.print(trQuotes)
          } else {
            //processing Float constants
            val printValue = x.escapedStringValue + (if (x.value.isInstanceOf[Float]) "F" else "") //correct printing of Float
            subPrinter.print(printValue)
          }

        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            subPrinter.print("@", tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
          }

          val printParantheses = specialTreeContext(tree)()
          codeInParantheses(printParantheses){subPrinter.print(tree)}; subPrinter.print(if (tree.isType) " " else ": ")
          printAnnot()

        case SelectFromTypeTree(qualifier, selector) =>
          subPrinter.print("(", qualifier, ")#", symbName(tree, selector))

        case CompoundTypeTree(templ) =>
          contextManaged(tree){
            subPrinter.print(templ)
          }

        case AppliedTypeTree(tp, args) =>
          //it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
          def containsByNameTypeParam =
            args exists {
                case AppliedTypeTree(Select(qual, name), _) => name.toString.trim.equals("<byname>")
                case _ => false
              }

          if (containsByNameTypeParam) {
            subPrinter.print("(")
            printRow(args.init, "(", ", ", ")")
            subPrinter.print(" => ", args.last, ")")
          } else {
            if (tp.exists {
              case Select(_, name) => compareNames(name, tpnme.REPEATED_PARAM_CLASS_NAME)
              case _ => false
            } && !args.isEmpty) {
              subPrinter.print(args(0), "*")
            } else if (tp match {
              case Select(_, name) => compareNames(name, tpnme.BYNAME_PARAM_CLASS_NAME)
              case _ => false
            }) {
              subPrinter.print("=> ", if (args.isEmpty) "()" else args(0))
            } else {
              subPrinter.print(tp);
              printRow(args, "[", ", ", "]")
            }
          }

        case ExistentialTypeTree(tpt, whereClauses) =>
          subPrinter.print("(", tpt);
          printColumn(whereClauses, " forSome { ", ";", "})")

        case tbt@TypeBoundsTree(lo, hi) => {
          val loDefault = "_root_.scala.Nothing"
          val hiDefault = "_root_.scala.Any"
          if (loDefault != lo.toString()) printOpt(" >: ", lo); if (hiDefault != hi.toString()) printOpt(" <: ", hi)
        }

        case emptyTree if emptyTree.toString == "<empty>" => // workaround as case EmptyTree does not work for all universes because of path depedent types

        case tree => super.printTree(tree)
      }
      //TODO remove
      if (printTypes && tree.isTerm && !tree.isEmpty) {
        subPrinter.print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    //Danger: it's overwritten method - can be problems with inheritance)
    def symbName(tree: Tree, name: Name, decoded: Boolean = decodeNames) = {
      val encName = name.encoded
      val decName = name.decoded
      def modifyEncoded(s: String) = if (decoded && (encName.contains("$u") ||
        (encName.contains("$") && decName.exists(ch => opSym.contains(ch)) && decName.exists(ch => !opSym.contains(ch)) && !excList.exists(str => decName.contains(str)))))
        "`%s`" format s else s

      if (compareNames(name, nme.CONSTRUCTOR)) "this"
      else modifyEncoded(quotedName(name, decoded))
    }

    val opSym = List('~', '=', '<', '>', '!', '#', '%', '^', '&', '|', '*', '/', '+', '-', ':', '\\', '?', '@')
    val excList = List("\\", "_*")

    override def print(args: Any*): Unit = {
      args foreach {
        arg =>
        //TODO repair issue with pattern matching, trees and vals of type Any
          if (arg.isInstanceOf[Tree]) { //problem with vars of type Any
          val treeArg = arg.asInstanceOf[Tree]
            printTree(treeArg)
          } else {
            arg match {
              case name: Name =>
                subPrinter.print(quotedName(name))
              case other => subPrinter.print(other)
            }
          }
      }
    }
  }

 }

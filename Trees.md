
Here's the type hierarchy of the `scalac`'s AST classes. Note that

*    `{}` is used to list subtypes, not to denote inclusion as in regular Scala code
*    `Function` and `ApplyDynamic` are a SymTree and a TermTree at the same time
*    `RefTree` is a `SymTree` and a `NameTree` at the same time
*    `SelectFromTypeTree` is a `RefTree` and a `TypTree` at the same time

```scala
    abstract class Tree extends TreeContextApiImpl with Attachable with Product {
      trait TermTree extends Tree with TermTreeApi {
        case class Block(stats: List[Tree], expr: Tree) extends TermTree with BlockApi
        case class Alternative(trees: List[Tree]) extends TermTree with AlternativeApi
        case class Star(elem: Tree) extends TermTree with StarApi
        case class UnApply(fun: Tree, args: List[Tree]) extends TermTree with UnApplyApi
        case class ArrayValue(elemtpt: Tree, elems: List[Tree]) extends TermTree
        case class Function(vparams: List[ValDef], body: Tree) extends SymTree with TermTree with FunctionApi
        case class ApplyDynamic(qual: Tree, args: List[Tree]) extends SymTree with TermTree
        case class Assign(lhs: Tree, rhs: Tree) extends TermTree with AssignApi
        case class AssignOrNamedArg(lhs: Tree, rhs: Tree) extends TermTree with AssignOrNamedArgApi
        case class If(cond: Tree, thenp: Tree, elsep: Tree) extends TermTree with IfApi
        case class Match(selector: Tree, cases: List[CaseDef]) extends TermTree with MatchApi
        case class Return(expr: Tree) extends SymTree with TermTree with ReturnApi
        case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree) extends TermTree with TryApi
        case class Throw(expr: Tree) extends TermTree with ThrowApi
        case class New(tpt: Tree) extends TermTree with NewApi
        case class Typed(expr: Tree, tpt: Tree) extends TermTree with TypedApi
        abstract class GenericApply extends TermTree with GenericApplyApi {
          case class TypeApply(fun: Tree, args: List[Tree]) extends GenericApply with TypeApplyApi
          case class Apply(fun: Tree, args: List[Tree]) extends GenericApply with ApplyApi
        }
        case class Super(qual: Tree, mix: TypeName) extends TermTree with SuperApi
        case class This(qual: TypeName) extends SymTree with TermTree with ThisApi
        case class ReferenceToBoxed(ident: Ident) extends TermTree with ReferenceToBoxedApi
        case class Literal(value: Constant) extends TermTree with LiteralApi
      }
      trait NameTree extends Tree with NameTreeApi {
        trait RefTree extends SymTree with NameTree with RefTreeApi { ... }
      }
      case class Annotated(annot: Tree, arg: Tree) extends Tree with AnnotatedApi
      case class CaseDef(pat: Tree, guard: Tree, body: Tree) extends Tree with CaseDefApi
      abstract class SymTree extends Tree with SymTreeContextApi {
        case class Import(expr: Tree, selectors: List[ImportSelector]) extends SymTree with ImportApi
        case class Template(parents: List[Tree], self: ValDef, body: List[Tree]) extends SymTree with TemplateApi
        case class Function(vparams: List[ValDef], body: Tree) extends SymTree with TermTree with FunctionApi
        case class ApplyDynamic(qual: Tree, args: List[Tree]) extends SymTree with TermTree
        trait RefTree extends SymTree with NameTree with RefTreeApi {
          case class Select(qualifier: Tree, name: Name) extends RefTree with SelectApi
          case class Ident(name: Name) extends RefTree with IdentContextApi
          case class SelectFromTypeTree(qualifier: Tree, name: TypeName) extends RefTree with TypTree with SelectFromTypeTreeApi
        }
        abstract class DefTree extends SymTree with NameTree with DefTreeApi {
          case class LabelDef(name: TermName, params: List[Ident], rhs: Tree) extends DefTree with TermTree with LabelDefApi
          abstract class MemberDef extends DefTree with MemberDefApi {
            case class PackageDef(pid: RefTree, stats: List[Tree]) extends MemberDef with PackageDefApi
            abstract class ImplDef extends MemberDef with ImplDefApi {
              case class ModuleDef(mods: Modifiers, name: TermName, impl: Template) extends ImplDef with ModuleDefApi
              case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template) extends ImplDef with ClassDefApi
            }
            abstract class ValOrDefDef extends MemberDef with ValOrDefDefApi {
              case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) extends ValOrDefDef with ValDefApi
              case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) extends ValOrDefDef with DefDefApi
            }
            case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree) extends MemberDef with TypeDefApi
          }
          case class Bind(name: Name, body: Tree) extends DefTree with BindApi
        }
      }
      trait TypTree extends Tree with TypTreeApi {
        case class SingletonTypeTree(ref: Tree) extends TypTree with SingletonTypeTreeApi
        case class SelectFromTypeTree(qualifier: Tree, name: TypeName) extends RefTree with TypTree with SelectFromTypeTreeApi
        case class CompoundTypeTree(templ: Template) extends TypTree with CompoundTypeTreeApi
        case class AppliedTypeTree(tpt: Tree, args: List[Tree]) extends TypTree with AppliedTypeTreeApi
        case class TypeBoundsTree(lo: Tree, hi: Tree) extends TypTree with TypeBoundsTreeApi
        case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree]) extends TypTree with ExistentialTypeTreeApi
        case class TypeTree() extends TypTree with TypeTreeContextApi
      }
    }
```

These two are not subclasses of `Tree`:

```scala
    case class Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree]) extends ModifiersApi with HasFlags
    case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int) extends ImportSelectorApi
```

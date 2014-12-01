package scala.print.plugin

import scala.tools.nsc.Global

trait Transformations
  extends UndoAutotupling
  with ExplicitUnitReturn
  with ExplicitImplicitTypes
  with EarlyInitializers
  with ForSome
  with UnnamedTypeParams
  with VarargsPatterns
  with WithGlobal
{
  import global._

  def transformations(afterParser: Tree, afterTyper: Tree, cu: CompilationUnit): String = {
    val source = cu.source.content.map(String.valueOf(_))
    // undoAutotupling(afterParser, afterTyper, source, cu)
    explicitUnitReturn(afterParser, source, cu)
    earlyInitializers(afterParser, source, cu)
    forSomeToWildcard(afterTyper, source, cu)
    unnamedTypeParams(afterTyper, source, cu)
    varargsPatterns(afterTyper, source, cu)
    explicitImplicitTypes(afterTyper, source, cu)
    source.mkString
  }

  /*
  def transformations(afterParser: Tree, cu: CompilationUnit): String = {
    val source = cu.source.content.map(String.valueOf(_))
    explicitUnitReturn(afterParser, source, cu)
    source.mkString
  }
  */
}


Options `-Xshow-phases -Ydebug` to display compiler phases:

    [info]              phase name  id  description
    [info]              ----------  --  -----------
    [info]                  parser   1  parse source into ASTs, perform simple desugaring
    [info] printSourceAfter_parser   2  
    [info]                   namer   3  resolve names, attach symbols to named trees
    [info]          packageobjects   4  load package objects
    [info]                   typer   5  the meat and potatoes: type the trees
    [info]  printSourceAfter_typer   6  
    [info]                  patmat   7  translate match expressions
    [info]          superaccessors   8  add super accessors in traits and nested classes
    [info]              extmethods   9  add extension methods for inline classes
    [info]                 pickler  10  serialize symbol tables
    [info]                xsbt-api  11  
    [info]               refchecks  12  reference/override checking, translate nested objects
    [info]                 uncurry  13  uncurry, translate function values to anonymous classes
    [info]               tailcalls  14  replace tail calls by jumps
    [info]              specialize  15  @specialized-driven class and method specialization
    [info]           explicitouter  16  this refs to outer pointers, translate patterns
    [info]                 erasure  17  erase types, add interfaces for traits
    [info]             posterasure  18  clean up erased inline classes
    [info]                lazyvals  19  allocate bitmaps, translate lazy vals into lazified defs
    [info]              lambdalift  20  move nested functions to top level
    [info]            constructors  21  move field definitions into constructors
    [info]                 flatten  22  eliminate inner classes
    [info]                   mixin  23  mixin composition
    [info]                 cleanup  24  platform-specific cleanups, generate reflective calls
    [info]                   icode  25  generate portable intermediate code
    [info]                 inliner  26  optimization: do inlining
    [info] inlineExceptionHandlers  27  optimization: inline exception handlers
    [info]                closelim  28  optimization: eliminate uncalled closures
    [info]                     dce  29  optimization: eliminate dead code
    [info]                     jvm  30  generate JVM bytecode
    [info]           xsbt-analyzer  31  
    [info]                terminal  32  The last phase in the compiler chain


    [info]              phase name  id  new flags
    [info]              ----------  --  ---------
    [info]                  parser   1  <method> private final protected case <deferred> <module> override <interface> implicit sealed abstract <mutable> <param> <package> <macro> <bynameparam/captured/covariant> <contravariant/inconstructor/label> absoverride <local> <java> <synthetic> <stable> <static> <caseaccessor> <defaultparam/trait> <bridge> <accessor> <superaccessor> <paramaccessor> <modulevar> lazy <is_error> <overloaded> <lifted> <existential/mixedin> <expandedname> <implclass/presuper> <trans_flag> <locked> <specialized> <defaultinit> <vbridge> <varargs> <triedcooking> <synchronized> <defaultmethod>
    [info] printSourceAfter_parser   2  
    [info]                   namer   3  
    [info]          packageobjects   4  
    [info]                   typer   5  
    [info]  printSourceAfter_typer   6  
    [info]                  patmat   7  
    [info]          superaccessors   8  [START] <notprivate>
    [info]              extmethods   9  
    [info]                 pickler  10  
    [info]                xsbt-api  11  
    [info]               refchecks  12  [START] <latemethod>
    [info]                 uncurry  13  
    [info]               tailcalls  14  
    [info]              specialize  15  [START] <latefinal> <notprivate>
    [info]           explicitouter  16  [START] <notprotected>
    [info]                 erasure  17  [START] <latedeferred> <lateinterface>
    [info]             posterasure  18  
    [info]                lazyvals  19  
    [info]              lambdalift  20  
    [info]            constructors  21  
    [info]                 flatten  22  
    [info]                   mixin  23  [START] <latemodule> <notoverride>
    [info]                 cleanup  24  
    [info]                   icode  25  
    [info]                 inliner  26  
    [info] inlineExceptionHandlers  27  
    [info]                closelim  28  
    [info]                     dce  29  
    [info]                     jvm  30  
    [info]           xsbt-analyzer  31  
    [info]                terminal  32  


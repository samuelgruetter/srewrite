srewrite plugin
===============

In order to compile Scala code written for `scalac` 2 with  [dotty](https://github.com/lampepfl/dotty/), some changes are necessary. The goal of this tool is to perform these changes automatically, with minimal differences in the source code.

Here's a list of these changes:

*    [DONE] explicit unit return types (ie. no "procedure syntax" for defs)
*    [DONE] explicit unit return types for abstract methods as well
*    [DONE] explicit types for implicit vals
*    [DONE] existential types (`forSome`) rewriting
*    [DONE] rewrite early initializers to late (normal) initializers and emit a big warning that it has to be reviewed manually
*    [TODO] add an `@volatile` annotation to all lazy vals
*    [Not needed] Autotupling. For instance, why does `List(1, 2, 3).toSet()` return `false`? See also this [discussion](https://groups.google.com/forum/#!topic/scala-debate/zwG8o2YzCWs) and this [blog post](http://dan.bodar.com/2013/12/04/wat-scala/).
*    [TODO] do not add newline to end of file if there's none
*    [TODO] imports are shadowed by class definitions in dotty (see `test/importShadowing.scala` for an example)
*    [TODO] In val defs of the form `val x = new SomeClass { refinements... }`, scalac infers a refined type as type of `x`, whereas dotty only infers `SomeClass`, so the rewrite tool should insert an explicit refined type for `x`.
*    [TODO] probably we need more transformations that we're not yet aware of

The challenge: Don't just reprint the whole code using any PrettyPrinter, loosing all formatting and comments. Modify the code as little as possible.

Demo: The [diff](https://github.com/samuelgruetter/srewrite-test/compare/diffdemo1original...diffdemo1) between original Scala source and source rewritten by srewrite.


### Build process

From srewrite directory run:

```shell
$ sbt assembly
```

Produced jar should be in project's target directory.

To add srewrite plugin jar to local ivy repo use:

```shell
$ sbt publish-local
```

Required jar should have similar path:

    ~/.ivy2/local/org.scala-lang.plugins/srewriteplugin_2.10/0.1.0/jars/srewriteplugin_2.10-assembly.jar

See <http://scala-sbt.org/release/docs/Getting-Started/Setup.html> for instructions to setup sbt.

### Usage

#### Sbt projects:

In the target project add to build.sbt (or build.scala) following option:

```scala
libraryDependencies += compilerPlugin("org.scala-lang.plugins" %% "srewriteplugin" % "0.1.0")
```

Compile the project: 

```shell
$ sbt compile
```

After the compilation generated sources should be in sourceFromAST folder (projectFolder/sourceFromAST).

#### Command-line:

To compile the project from the command-line use:

```shell
$ scalac -Xplugin:/path/to/jar/srewriteplugin_2.10-assembly.jar hello/world/*.scala
```

### Options

dir-name - setup custom name for folder with regenerated sources

```scala
scalacOptions += "-P:srewriteplugin:dir-name:printAST"
```

base-dir - setup custom path for regenerated sources 

```scala
scalacOptions += "-P:srewriteplugin:base-dir:/path/for/generated/sources"
```

oversrc - overwrite original sources with generated during the compilation.

```scala
scalacOptions += "-P:srewriteplugin:oversrc"
```

### Example

```shell
$ scalac -Xplugin:path/to/jar/srewriteplugin_2.10-assembly.jar -P:srewriteplugin:base-dir:/path/to/generated/sources -P:srewriteplugin:dir-name:source-hello-world hello/world/*.scala
```

Regenerated sources should be in `sourceFromAST` folder.

A sample sbt configuration can be found in srewrite-test project: <https://github.com/samuelgruetter/srewrite-test>

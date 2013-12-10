printPlugin
============

Scala compiler plugin:

- use sprinter library
- generate project sources based on program's ASTs

### Build process

From printplugin directory run:

```shell
$ sbt assembly
```

Produced jar should be in project's target directory.

To add printPlugin jar to local ivy repo use:

```shell
$ sbt publish-local
```

Required jar should have similar path:
/path/to/.ivy2/local/org.scala-lang.plugins/printplugin_2.10/0.2.0/jars/printplugin_2.10-assembly.jar

See <http://scala-sbt.org/release/docs/Getting-Started/Setup.html> for instructions to setup sbt.

### Usage

#### Sbt projects:

In the target project add to build.sbt (or build.scala) following option:

```scala
libraryDependencies += compilerPlugin("org.scala-lang.plugins" %% "printplugin" % "0.2.0")
```

Compile the project: 

```shell
$ sbt compile
```

After the compilation generated sources should be in sourceFromAST folder (projectFolder/sourceFromAST).

#### Command-line:

To compile the project from the command-line use:

```shell
$ scalac -Xplugin:/path/to/jar/printplugin-assembly-0.2.0.jar hello/world/*.scala
```

### Options

dir-name - setup custom name for folder with regenerated sources

```scala
scalacOptions += "-P:printplugin:dir-name:printAST"
```

base-dir - setup custom path for regenerated sources 

```scala
scalacOptions += "-P:printplugin:base-dir:/path/for/generated/sources"
```

oversrc - overwrite original sources with generated during the compilation.

```scala
scalacOptions += "-P:printplugin:oversrc"
```

### Example

```shell
$ scalac -Xplugin:path/to/jar/printplugin_2.10-assembly.jar -P:printplugin:base-dir:/path/to/generated/sources -P:printplugin:dir-name:source-hello-world hello/world/*.scala
```

Regenerated sources should be in sourceFromAST folder.

sbt configuration can be found in treePrintTester project - <https://github.com/VladimirNik/treePrintTester>

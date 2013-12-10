import sbt._
import Keys._
import Configurations.CompilerPlugin
import sbtassembly.Plugin._
import AssemblyKeys._

object SourcePrinter extends Build {
  val myPluginName = "srewriteplugin"
  
  val jarsToExclude = Seq("scala-reflect.jar", "scala-library.jar", "scala-compiler.jar")
  val buildSettings = Defaults.defaultSettings ++
    Seq(
      organization := "org.scala-lang.plugins",
      version := "0.1.0",
      scalaVersion := "2.10.2"
    )
    val assemblyProjectSettings = Seq(
      excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
        cp filter {cpj => jarsToExclude.contains(cpj.data.getName)}
      }
    ) 

    val srewriteplugin = Project(myPluginName, file("."),
      settings = buildSettings ++ assemblySettings ++ assemblyProjectSettings ++
      addArtifact(Artifact(myPluginName, "assembly"), sbtassembly.Plugin.AssemblyKeys.assembly)) settings (
         name := myPluginName,

      //crossVersion := CrossVersion.full,
      //exportJars := true,
      libraryDependencies <++= scalaVersion apply dependencies
    ) 

    def dependencies(sv: String) = Seq(
      "org.scala-lang" % "scala-compiler" % sv,
      "org.scala-lang" %% "sprinter" % "0.2.0"
    )
}

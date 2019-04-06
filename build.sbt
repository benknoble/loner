import Dependencies._
import sbtassembly.AssemblyPlugin.defaultShellScript

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "org.benknoble"
ThisBuild / organizationName := "benknoble"

enablePlugins(SiteScaladocPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "loner",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
    assemblyJarName in assembly := s"${name.value}-${version.value}",
    libraryDependencies += scalaTest % Test,
    siteSubdirName in ScalaUnidoc := "api",
    addMappingsToSiteDir(
      mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc)
  )
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(ebnf)
  .dependsOn(ebnf)

lazy val ebnf = (project in file("ebnf"))
  .settings(
    name := "ebnf",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
    assemblyJarName in assembly := s"${name.value}-${version.value}",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += parserCombinators
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

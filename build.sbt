import Dependencies._
import sbtassembly.AssemblyPlugin.defaultShellScript

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.2.0"
ThisBuild / organization     := "org.benknoble"
ThisBuild / organizationName := "benknoble"

enablePlugins(SiteScaladocPlugin)

lazy val loner = (project in file("."))
  .settings(
    name := "loner",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
    assemblyJarName in assembly := s"${name.value}-${version.value}",
    libraryDependencies += scalaTest % Test
  )
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

lazy val Loner = config("root")
lazy val Ebnf = config("ebnf")

lazy val scaladocSiteProjects = List(
    (loner, Loner),
    (ebnf, Ebnf))

lazy val scaladocSiteSettings =
  scaladocSiteProjects.flatMap { case (project, conf) =>
    SiteScaladocPlugin.scaladocSettings(
        conf,
        mappings in (Compile, packageDoc) in project,
        s"api/${project.id}")
  }

lazy val scaladocSite = (project in file("site"))
  .settings(
    scaladocSiteSettings,
    git.remoteRepo := "git@github.com:benknoble/loner.git",
    ghpagesNoJekyll := true
  )
  .enablePlugins(GhpagesPlugin)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

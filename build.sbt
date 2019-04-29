import sbtassembly.AssemblyPlugin.defaultShellScript

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.2.0"
ThisBuild / organization     := "org.benknoble"
ThisBuild / organizationName := "benknoble"

// can't be in dependencies because of scalaJSVersion for the moment
lazy val scalajsStubs = "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"

enablePlugins(SiteScaladocPlugin)

lazy val loner = (crossProject.crossType(CrossType.Pure) in file("."))
  .settings(
    name := "loner",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
    assemblyJarName in assembly := s"${name.value}-${version.value}",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5"
  )
  .jvmSettings(
    libraryDependencies += scalajsStubs
  )
  .jsSettings(
  )
  .aggregate(ebnf)
  .dependsOn(ebnf)

lazy val lonerJVM = loner.jvm
lazy val lonerJS = loner.js

lazy val ebnf = (crossProject.crossType(CrossType.Pure) in file("ebnf"))
  .settings(
    name := "ebnf",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
    assemblyJarName in assembly := s"${name.value}-${version.value}",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1"
  )
  .jvmSettings(
    libraryDependencies += scalajsStubs
  )
  .jsSettings(
  )

lazy val ebnfJVM = ebnf.jvm
lazy val ebnfJS = ebnf.js

lazy val Loner = config("root")
lazy val Ebnf = config("ebnf")

lazy val scaladocSiteProjects = List(
    (lonerJVM, Loner),
    (ebnfJVM, Ebnf))

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

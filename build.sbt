import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "org.benknoble"
ThisBuild / organizationName := "benknoble"

lazy val root = (project in file("."))
  .settings(
    name := "loner",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += parserCombinators
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

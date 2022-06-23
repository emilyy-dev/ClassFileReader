ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organization := "io.github.emilyy-dev"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.2.12"
lazy val classFileReader = (project in file("."))
  .settings(
    name := "ClassFileReader",
    libraryDependencies += scalatest % Test
  )

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / libraryDependencies += "org.scala-lang" %% "toolkit" % "0.2.0"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M7" % Test

lazy val root = (project in file("."))
  .settings(
    name := "day-7"
  )

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2024-scala",
    idePackagePrefix := Some("com.adventofcode"),
    libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0")
  )

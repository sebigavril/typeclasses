import sbt._
import Keys._

object HelloBuild extends Build {

  version      := "1.0"
  scalaVersion := "2.11.8"

  lazy val typeclassesTalks = Project(id = "typeclasses-talks", base = file("."))
                                .aggregate(mathInheritance, mathTypeclasses)

  lazy val mathInheritance = Project(id = "math-inheritance", base = file("math-inheritance"))
                               .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test")

  lazy val mathTypeclasses = Project(id = "math-typeclasses", base = file("math-typeclasses"))
                               .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test")
}

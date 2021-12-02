name := "replcalc"

version := "0.1"
scalaVersion := "3.1.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding",
  "utf8"
)

libraryDependencies ++= Seq(
  //Test dependencies
  "org.scalameta" %% "munit" % "0.7.26" % "test"
)

testFrameworks += new TestFramework("munit.Framework")

Test / parallelExecution := true
fork := true
Test / fork := true

developers := List(
  Developer("makingthematrix", "Maciej Gorywoda", "maciej.gorywoda@wire.com", url("https://github.com/makingthematrix"))
)

licenses := Seq("GPL 3.0" -> url("https://www.gnu.org/licenses/gpl-3.0.en.html"))

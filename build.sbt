name := "feyn"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test

testFrameworks += new TestFramework("minitest.runner.Framework")

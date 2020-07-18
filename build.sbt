enablePlugins(ScalaJSPlugin)

name := "feyn"

version := "0.1"

scalaVersion := "2.13.2"

scalaJSUseMainModuleInitializer := false

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "1.0.0",
  "io.monix" %%% "minitest" % "2.8.2" % Test
)

testFrameworks += new TestFramework("minitest.runner.Framework")

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

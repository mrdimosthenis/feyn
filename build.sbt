enablePlugins(ScalaJSPlugin)

name := "feyn"

version := "0.1"

scalaVersion := "2.13.2"

scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "1.0.0",
  "com.lihaoyi" %%% "scalatags" % "0.9.1",
  "org.akka-js" %%% "akkajsactor" % "2.2.6.5",
  "io.monix" %%% "minitest" % "2.8.2" % Test
)

testFrameworks += new TestFramework("minitest.runner.Framework")

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

name := "todo"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"
libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.3" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")


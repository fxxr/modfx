name := "modfx"
version := "0.1"
scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.2.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0", 
  "com.github.pathikrit" %% "better-files" % "3.5.0",
  "com.github.javaparser" % "javaparser-core" % "3.6.14",
  "com.lihaoyi" %% "utest" % "0.5.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions += "-Ypartial-unification"
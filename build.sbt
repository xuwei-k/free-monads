organization := "com.timperrett"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.0.13")

name := "free-logging"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0-M5"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.4"

testFrameworks += new TestFramework(
  "org.scalameter.ScalaMeterFramework")

logBuffered := false

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:_")

scalaVersion := "2.10.3"

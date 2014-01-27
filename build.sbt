organization := "com.timperrett"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.0.13")

val root = Project("free-logging", file(".")).dependsOn(
  ProjectRef(uri("git://github.com/scalaz/scalaz.git#88e0698ea"), "core")
)

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.4"

testFrameworks += new TestFramework(
  "org.scalameter.ScalaMeterFramework")

logBuffered := false

scalacOptions ++= Seq(
  "-feature", 
  "-language:postfixOps", 
  "-language:implicitConversions")

scalaVersion := "2.10.3"

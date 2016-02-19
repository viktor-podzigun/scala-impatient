
name := "scala-impatient"

organization := "com.github.viktor-podzigun"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

//fork := false

// to run coverage tests use:
//
// activator clean coverage test coverageReport
//
//
//coverageEnabled := true

coverageMinimum := 80

coverageFailOnMinimum := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.7" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

resolvers ++= Seq(
//  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
//  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

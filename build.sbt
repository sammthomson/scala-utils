name := "scala-utils"

organization := "com.samthomson"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "com.chuusai" %% "shapeless" % "2.0.0",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

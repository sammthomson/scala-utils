name := "scala-utils"

organization := "org.samthomson"

version := "0.2"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

bintrayReleaseOnPublish in ThisBuild := false

credentials := (
    if (isSnapshot.value)
      Seq(Credentials(Path.userHome / ".bintray" / ".artifactory"))
    else
      credentials.value
    )

publishTo := (
    if (isSnapshot.value)
      Some("Artifactory Realm" at "https://oss.jfrog.org/artifactory/oss-snapshot-local")
    else
      publishTo.value
    )

bintrayReleaseOnPublish in ThisBuild := false

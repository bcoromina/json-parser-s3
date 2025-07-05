val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "json-parser-s3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,


    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

)

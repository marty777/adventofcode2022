val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
	scalacOptions := Seq("-unchecked", "-deprecation"),

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )

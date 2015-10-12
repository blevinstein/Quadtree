scalaVersion := "2.11.4"

lazy val root = (project in file(".")).
    settings(
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"
    )

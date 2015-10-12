scalaVersion := "2.11.4"

lazy val root = (project in file(".")).
    settings(
      name := "Quadtree",
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"
    )

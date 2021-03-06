scalaVersion := "2.11.7"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"


scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"


scalacOptions ++= Seq("-deprecation", "-feature", "-Xlog-implicits")

lazy val grow = taskKey[Unit](
  "Run qt grow, a cellular automata evolutionary algorithm.")

grow := { (runMain in Compile).toTask(" com.blevinstein.qt.grow.Driver").value }

lazy val sim = taskKey[Unit]("Run qt sim, a basic simulation engine.")

sim := { (runMain in Compile).toTask(" com.blevinstein.qt.sim.Driver").value }


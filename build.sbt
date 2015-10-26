scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

scalacOptions ++= Seq("-deprecation", "-feature")

lazy val grow = taskKey[Unit](
  "Run qt grow, a cellular automata evolutionary algorithm.")

grow := { (runMain in Compile).toTask(" com.blevinstein.qt.grow.Driver").value }

lazy val sim = taskKey[Unit]("Run qt sim, a basic simulation engine.")

sim := { (runMain in Compile).toTask(" com.blevinstein.qt.sim.Driver").value }



scalaVersion := "2.12.10"
name := "auction"
organization := "org.mkmks"
version := "1.0"
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0" 
libraryDependencies += "co.fs2" %% "fs2-core" % "2.2.1"
libraryDependencies += "co.fs2" %% "fs2-io" % "2.2.1"

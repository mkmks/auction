
scalaVersion := "2.13.1"
name := "auction"
organization := "org.mkmks"
version := "1.0"
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-v", "3")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-r", "5")
libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-M1"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0" 
libraryDependencies += "co.fs2" %% "fs2-core" % "2.2.1"
libraryDependencies += "co.fs2" %% "fs2-io" % "2.2.1"

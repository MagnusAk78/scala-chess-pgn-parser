scalaVersion := "2.13.1"

name := "scala-chess-pgn-parser"
version := "0.1"

//Parser combinators are no longer part of scala default library
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

// Scalatest and scalactic (The dependency on Scalactic, ScalaTest's sister library focused on quality through
// types, is recommended but not required.)
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

//Compile with -deprecation and -feature flags
scalacOptions ++= Seq("-deprecation", "-feature")
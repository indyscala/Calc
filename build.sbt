name := "org.indyscala.parboiled.UnitConverter"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.0.0",
  "org.scala-lang" % "scala-reflect" % "2.11.1",
  "org.specs2" %% "specs2" % "2.4.2" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")


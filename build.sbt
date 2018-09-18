name := "data_heretic_compiler"

version := "0.1"

scalaVersion := "2.12.6"

val v = new {
  val circeVersion = "0.9.3"
  val scalatestVersion = "3.0.5"
}

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % v.circeVersion,
  "io.circe" %% "circe-generic"% v.circeVersion,
  "io.circe" %% "circe-parser"% v.circeVersion,
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.github.pathikrit" %% "better-files" % "3.6.0",
  "org.scalactic" %% "scalactic" % v.scalatestVersion,
  "org.scalatest" %% "scalatest" % v.scalatestVersion % "test"
)
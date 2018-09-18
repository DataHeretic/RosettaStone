package dataheretic.compiler
import better.files._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object SimpleRunner extends App {
  val dhDirRoot = args(0)

  val project = new Project(File(dhDirRoot))

  project.verifyMigrationFilenameCorrectness
  project.verifyVersionUniqueness

  println (project.getMigrationsFromFolder.asJson)
}

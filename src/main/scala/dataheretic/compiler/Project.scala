package dataheretic.compiler

import better.files._
import DHCompiler.{AST, Rules}
import fastparse.core.Parsed

class Project (root: File) {

  val migrationsRoot = root / "migrations"

  val migrationFileFormat = """^([0-9])+_[a-zA-Z0-9_-]+\.dh"""

  lazy val getMigrationsFromFolder: Seq[AST.Migration] =
    migrationsRoot.children.filter {
      _.name.matches(migrationFileFormat)
    }.map { file =>
      val contents = file.lines.mkString("\n")
      val version = migrationFileFormat.r.findFirstMatchIn(file.name).map {
        _.group(1).toInt
      }.get
      val parsed = Rules.Migrations.MigrationClause(version).parse(contents)

      parsed match {
        case Parsed.Success(migration, _) => migration
        case f: Parsed.Failure[_, _] =>
          throw new Exception(s"Error: Failed to parse ${file.name}", new Exception(f.toString()))
      }
    }.toSeq sortBy (_.version)

  lazy val verifyVersionUniqueness =
    getMigrationsFromFolder.groupBy(_.version).find (_._2.length > 1) match {
      case Some((version, _)) =>
        throw new Exception(s"Duplicate migration version found: $version")
      case None => // noop
    }

  lazy val verifyMigrationFilenameCorrectness =
    migrationsRoot.children.filterNot { _.name.matches(migrationFileFormat) }.toList match {
      case Nil => // good
      case bad: List[File] =>
        throw new Exception(s"Invalid files found in the migrations folder: ${bad}")
    }
}

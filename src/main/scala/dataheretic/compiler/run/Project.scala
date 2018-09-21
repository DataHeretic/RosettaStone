package dataheretic.compiler.run

import better.files._
import dataheretic.compiler.DHCompiler.AST.DataHereticAST
import dataheretic.compiler.DHCompiler.{AST, Rules, Utils}
import fastparse.core.Parsed

import scala.util.Try

class Project (rootDir: File) {

  val migrationsRoot = rootDir / "migrations"

  val migrationFileFormat = """^([0-9])+_[a-zA-Z0-9_-]+\.dh$"""

  lazy val verifyAndGet: Try[AST.DataHereticAST] = {
    verifyMigrationFilenameCorrectness
    verifyVersionUniqueness
    getAST
  }

  lazy val getAST: Try[DataHereticAST] =
    getMigrationsFromFolder map {
      case migrationSeq =>
        DataHereticAST(migrations = migrationSeq)
    }

  lazy val getMigrationsFromFolder: Try[Seq[AST.Migration]] = Try {
    migrationsRoot.children.filter {
      _.name.matches(migrationFileFormat)
    }.map { file =>
      val contents = file.lines.mkString("\n")
      val version = migrationFileFormat.r.findFirstMatchIn(file.name).map {
        _.group(1).toInt
      }.get
      val parsed = Utils.wholeInput(Rules.Migrations.MigrationClause(version)).parse(contents)

      parsed match {
        case Parsed.Success(migration, _) => migration
        case f: Parsed.Failure[_, _] =>
          throw new Exception(s"Error: Failed to parse ${file.name}", new Exception(f.toString()))
      }
    }.toSeq sortBy (_.version)
  }

  lazy val verifyVersionUniqueness: Try[Unit] =
    getMigrationsFromFolder map {
      _.groupBy(_.version).find(_._2.length > 1) match {
        case Some((version, _)) =>
          throw new Exception(s"Duplicate migration version found: $version")
        case None => // noop
      }
    }

  lazy val verifyMigrationFilenameCorrectness =
    migrationsRoot
      .children
      .filter { f =>
        f.name.endsWith(".dh") || // a misguided attempt?
        f.name.endsWith(".sql")   // someone could think it's sql scripts?
      }
      .filterNot { _.name.matches(migrationFileFormat) }.toList match {
        case Nil => // good
        case bad: List[File] =>
          throw new Exception(s"Invalid migration files detected in the migrations folder: ${bad}")
      }
}

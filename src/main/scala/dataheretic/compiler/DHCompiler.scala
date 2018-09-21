package dataheretic.compiler

import dataheretic.compiler.DHCompiler.AST.DataHereticAST
import fastparse.all._

import scala.util.Try

object DHCompiler {

  object CommonTokens {
    val INDENT = "  " // two spaces
  }

  object SQLTestsTokens {
    val ENSURING = "ENSURING"
    val IT = "it"
    val GIVEN = "given"
    val WHEN = "when"
    val THEN = "then"
  }

  object MigrationTokens {
    val MIGRATION = "MIGRATION"
    val UP = "UP"
    val DOWN = "DOWN"
  }

  object Rules {
    import Utils._

    object Common {
      import CommonTokens._
      def ▶▶▶(in: Int) = INDENT rep (min = in, max = in)
      val ▶▶ = (" " | "\t") rep 1
      val ▶▶▼ = ▶▶.? ~ ("\n" | "\r\n")
      val ▶▶▼▶▶ = ▶▶▼ ~ ▶▶.?
      val ▶▶▼▼ = ▶▶▼.rep(1)

      val Line = CharPred(!"\r\n".contains(_)).rep(1).!
      def IndentedLine (in: Int) = ▶▶▶(in) ~ Line
    }

    object SQLTests {
      import Common._
      import SQLTestsTokens._

      // Sans whitespace nor backticks
      val LiteralCell =
        CharPred(!"` \t\n\r".contains(_)).rep(1).!

      // Anything within the backticks goes
      val QuotedCell =
        "`" ~ CharPred(_ != '`').rep.! ~ "`"

      // Always captures
      val Cell = QuotedCell | LiteralCell

      def HeaderRow (in: Int) = ▶▶▶(in) ~ Cell ~ (▶▶ ~ Cell).rep map { case (h, hs) => h +: hs }
      def ResultRow (in: Int) = ▶▶▶(in) ~ Cell ~ (▶▶ ~ Cell).rep map { case (r, rs) => r +: rs }

      def Results (in: Int) = P(HeaderRow(in) ~ (▶▶▼ ~ ResultRow(in)).rep) filter {
        case (header, rows) => verifyColumnCount(header, rows)
      } map {
        case (header, rows) =>
          rows map { row => (header zip row).toMap }
      }

      def It      (in: Int) =
        ▶▶▶(in) ~ IT ~ ▶▶ ~ Line

      def Given   (in: Int) =
        ▶▶▶(in) ~ GIVEN ~ (▶▶▼▼ ~ IndentedLine(in++)).rep(1).map { _ reduce (_.trim + "\n" + _.trim)}

      def When    (in: Int) =
        ▶▶▶(in) ~ WHEN  ~ (▶▶▼▼ ~ IndentedLine(in++)).rep(1).map { _ reduce (_.trim + "\n" + _.trim)}

      def Then    (in: Int) =
        ▶▶▶(in) ~ THEN ~ ▶▶▼▼ ~ Results(in++)

      def TestClause(in: Int) =
        It(in) ~ ▶▶▼▼ ~
        (Given(in) ~ ▶▶▼▼).? ~
        When(in) ~ ▶▶▼▼ ~
        Then(in) map { AST.TestCase.tupled }

      def Ensuring(in: Int): Parser[Seq[AST.TestCase]] =
        ▶▶▶(in) ~ ENSURING ~ (▶▶▼▼ ~ TestClause(in++)).rep(1)
    }

    object Migrations {
      import Common._
      import MigrationTokens._
      import SQLTests._

      lazy val Meta =
        MIGRATION ~ ▶▶ ~ Line

      lazy val Up =
        UP ~ (▶▶▼▼ ~ IndentedLine(1)).rep(1) map { _ reduce (_ + "\n" + _)}

      lazy val Down =
        DOWN ~ (▶▶▼▼ ~ IndentedLine(1)).rep(1) map { _ reduce (_ + "\n" + _)}

      def MigrationClause(version: Int) =
         Meta ~ ▶▶▼▼ ~
         Up ~ ▶▶▼▼ ~
         Down ~ (▶▶▼▼ ~ Ensuring(0)).? ~ ▶▶▼▶▶.rep map {
           case (description, upSQL, downSQL, tests) => AST.Migration(version, description, upSQL, downSQL, tests)
         }
    }
  }

  object AST {
    case class TestCase (it: String, given: Option[String], when: String, `then`: Seq[Map[String, String]])
    case class Migration (version: Int, description: String, upSQL: String, downSQL: String, tests: Option[Seq[TestCase]])
    case class DataHereticAST (migrations: Seq[Migration])
  }

  object Utils {

    implicit class IntExtension(v: Int) {
      val ++ = v + 1
      val -- = v - 1
    }

    def verifyColumnCount (header: Seq[_], rows: Seq[Seq[_]]): Boolean = {
      val columns = header.size
      rows forall { _.size == columns }
    }

    def wholeInput[T](parser: Parser[T]) = Start ~ parser ~ End

    def ParseToAST (migrationsRaw: Seq[String]): Try[DataHereticAST] = Try {
      DataHereticAST(
        migrations = migrationsRaw map (wholeInput(Rules.Migrations.MigrationClause(0)).parse(_).get.value)
      )
    }
  }
}
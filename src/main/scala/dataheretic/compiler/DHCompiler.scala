package dataheretic.compiler

import fastparse.all._

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

  implicit class IntExtension(v: Int) {
    val ++ = v + 1
    val -- = v - 1
  }

  object Rules {

    import Utils._

    object General {
      import CommonTokens._
      def ▶▶▶(in: Int) = INDENT rep (min = in, max = in)
      val ▶▶ = (" " | "\t") rep 1
      val ▼▼ = ▶▶.? ~ ("\n" | "\r\n")

      val Line = CharPred(!"\r\n".contains(_)).rep(1).!
      def IndentedLine (in: Int) = ▶▶▶(in) ~ Line
    }


    object SQLTests {

      import General._
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

      def Results (in: Int) = P(HeaderRow(in) ~ (▼▼ ~ ResultRow(in)).rep) filter {
        case (header, rows) => verifyColumnCount(header, rows)
      } map {
        case (header, rows) =>
          rows map { header zip _ }
      }

      def It      (in: Int) =
        ▶▶▶(in) ~ IT ~ ▶▶.? ~ Line

      def Given   (in: Int) =
        ▶▶▶(in) ~ GIVEN ~ (▼▼ ~ IndentedLine(in++)).rep(1).map { _ reduce (_.trim + "\n" + _.trim)}

      def When    (in: Int) =
        ▶▶▶(in) ~ WHEN  ~ (▼▼ ~ IndentedLine(in++)).rep(1).map { _ reduce (_.trim + "\n" + _.trim)}

      def Then    (in: Int) =
        ▶▶▶(in) ~ THEN ~ ▼▼ ~ Results(in++)

      def TestClause(in: Int) =
        It(in) ~ ▼▼ ~ (Given(in) ~ ▼▼).? ~ When(in) ~ ▼▼ ~ Then(in) map { AST.TestCase.tupled }

      def Ensuring(in: Int): Parser[Seq[AST.TestCase]] =
        ▶▶▶(in) ~ ENSURING ~ (▼▼.rep(1) ~ TestClause(in++)).rep(1)
    }

    object Migrations {
      import General._
      import MigrationTokens._
      import SQLTests._

      def Meta =
        MIGRATION ~ "(" ~ CharIn('0' to '9').! ~ ")" ~ ▶▶ ~ Line map {
          case (version, description) => (version.toInt, description)
        }

      def Up =
        UP ~ (▼▼.rep(1) ~ IndentedLine(1)).rep(1) map { _ reduce (_ + "\n" + _)}

      def Down =
        DOWN ~ (▼▼.rep(1) ~ IndentedLine(1)).rep(1) map { _ reduce (_ + "\n" + _)}

      def Migration =
         Meta ~ ▼▼.rep(1) ~
         Up ~ ▼▼.rep(1) ~
         Down ~ ▼▼.rep(1) ~
         Ensuring(0).? ~ ▼▼.rep map { AST.Migration.tupled }
    }
  }

  object AST {
    case class TestCase (it: String, given: Option[String], when: String, `then`: Seq[Seq[(String, String)]])
    case class Migration (version: Int, description: String, upSQL: String, downSQL: String, tests: Option[Seq[TestCase]])
  }

  object Utils {
    def verifyColumnCount (header: Seq[_], rows: Seq[Seq[_]]): Boolean = {
      val columns = header.size
      rows forall { _.size == columns }
    }
  }

}

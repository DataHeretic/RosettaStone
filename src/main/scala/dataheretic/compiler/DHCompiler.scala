package dataheretic.compiler

import fastparse.all._

object DHCompiler {

  object CommonTokens {
    val INDENT = "  " // two spaces
    val UP = "UP"
    val DOWN = "DOWN"
  }

  object SQLTestsTokens {
    val ENSURING = "ENSURING"
    val IT = "it"
    val GIVEN = "given"
    val WHEN = "when"
    val THEN = "then"
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
        ▶▶▶(in) ~ ENSURING ~ (▼▼.rep ~ TestClause(in++)).rep(1)
    }

    object SQLMigrations {
//      def Up
    }
  }

  object AST {
    case class TestCase (it: String, given: Option[String], when: String, `then`: Seq[Seq[(String, String)]])
  }

  object Utils {
    def verifyColumnCount (header: Seq[_], rows: Seq[Seq[_]]): Boolean = {
      val columns = header.size
      rows forall { _.size == columns }
    }
  }

}

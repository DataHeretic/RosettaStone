package dataheretic.compiler
import fastparse.all._

object DHCompiler {

  object Tokens {
    val INDENT = "  " // two spaces
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

    import Tokens._

    def ▶▶▶(in: Int) = INDENT.rep(min = in, max = in)
    val ▶▶ = CharPred(" \t".contains(_)).rep(1)
    val ▼▼ = "\n" | "\r\n"

    // Sans whitespace nor backticks
    val LiteralCell =
      CharPred(!"` \t\n\r".contains(_)).rep(1).!

    // Anything within the backticks goes
    val QuotedCell =
      "`" ~ CharPred(_ != '`').rep.! ~ "`"

    // Always captures
    val Cell = QuotedCell | LiteralCell

    def HeaderRow (in: Int) = ▶▶▶(in) ~ Cell ~ (▶▶ ~ Cell).rep
    def ResultRow (in: Int) = ▶▶▶(in) ~ (Cell ~ (▶▶ ~ Cell).rep).?

    def Results (in: Int) = P(HeaderRow(in).rep(1).! ~ (▼▼ ~ ResultRow(in).!).rep(0)) map {
      case (head, rows) => head -> rows
    }

    def Then(in: Int) = P(▶▶▶(in) ~ THEN ~ ▼▼ ~ Results(in++) ~ End)
  }

}

package dataheretic.compiler
import fastparse.all._

object DHCompiler {

  object Tokens {
    val NL = '\n'
    val INDENT = "  " // two spaces
    val IT = "it"
    val GIVEN = "given"
    val WHEN = "when"
    val THEN = "then"
  }

  implicit class IntMagics (v: Int) {
    val ++ = v + 1
    val -- = v - 1
  }

  object Rules {

    import Tokens._

    def ▶▶▶(in: Int) = INDENT.rep(min = in, max = in)
    val ▶▶ = ▶▶▶(1)
    val ▼▼ = CharIn(NL :: Nil)

    val Cell =
      CharIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq('_', '-')).rep(0) |
      ("`" ~ AnyChar.rep(0) ~ "`")

    def Headers   (in: Int) = P(▶▶▶(in--) ~ (▶▶ ~ Cell.!).rep(1))
    def ResultRow (in: Int) = P(▶▶▶(in--) ~ (▶▶ ~ Cell.!).rep(1))

    def Results (in: Int) = P(Headers(in).rep(1).! ~ (▼▼ ~ ResultRow(in).!).rep(0)) map {
      case (head, rows) => head -> rows
    }

    def Then(in: Int) = P(▶▶▶(in) ~ THEN ~ ▼▼ ~ ▶▶▶(in++) ~ Results(in++) ~ End)
  }

}

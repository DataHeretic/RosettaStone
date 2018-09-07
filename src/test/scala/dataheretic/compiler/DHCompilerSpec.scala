package dataheretic.compiler

import fastparse.all._
import fastparse.core.Parsed
import org.scalatest.{FunSpec, Matchers}
import DHCompiler._

class DHCompilerSpec extends FunSpec with Matchers {

  it ("has validates the rules") {
    rule("validate a 1-length indent") {
      Rules.▶▶▶(1)
    }()(bad =
      "",
      " ",
      Tokens.INDENT + " "
    )

    rule("validate spacing rule between tokens") {
      "a" ~ Rules.▶▶ ~ "b"
    }(good =
      "a b",
      "a    b",
      "a\tb",
      "a \tb"
    )(bad =
      "ab",
      " ab",
      "ab ",
      "a\nb",
      "a \nb"
    )

    rule("validate the cell") {
      Rules.LiteralCell ~ End | Start ~ Rules.QuotedCell
    }(good =
      "absd",
      "``",
      "` \n\t\r`",
      "`dasd asdasd\t`"
    )(bad =
      "",
      " hi",
      "h i",
      "hi ",
      "\t",
      " ",
      "`",
      "`sdfsd",
      "` "
    )

    // Validate indentation-sensitive rules
    for (i <- 0 to 2) {
      val indent = (0 to (i-1)).foldLeft("") { (acc, _) => acc + Tokens.INDENT }

      rule(s"validate ${i}-width indent") {
        Rules.▶▶▶(i)
      }(good =
        indent
      )(bad =
        indent + Tokens.INDENT,
        indent + " "
      )

      rule(s"validate header row with ${i}-indentation") {
        Rules.HeaderRow(i)
      }(good =
        s"${indent}this",
        s"${indent}`th at`",
        s"${indent}something  or  `other`",
        s"${indent}`even\nnewlines`  are  accepted"
      )(bad =
//        " single space indent",
//        s"${indent} plus a single space indent",
        s"${indent}",
        s"${indent}${Tokens.INDENT}overly indented row",
        s"${indent}\tha"
      )
    }
  }

  def rule[T](description: String)(parser: Parser[T])(good: String*)(bad: String*) {
    println(s"> Rule: $description")
    val strictRule = Start ~ parser ~ End
    println(s"\t > With parser: $strictRule")
    good foreach { s =>
      println(s"\t\t> Good:\t'$s'")
      strictRule.parse(s) match {
        case _: Parsed.Success[T, _, _] =>
        case f: Parsed.Failure[T, _] =>
          println (s"Unexpectedly fail on '$s'")
          println (f)
          throw new IllegalArgumentException(f.msg)
      }
    }

    bad foreach { s =>
      println(s"\t\t> Bad:\t'$s'")
      strictRule.parse(s) match {
        case _: Parsed.Failure[T, _] => // good!
        case _ => throw new IllegalArgumentException(s"Unexpectedly succeed on '$s'")
      }
    }
  }

}

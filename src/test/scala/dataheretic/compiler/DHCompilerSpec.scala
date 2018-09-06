package dataheretic.compiler

import fastparse.all._
import fastparse.core.Parsed
import org.scalatest.{FunSpec, Matchers}
import DHCompiler._

class DHCompilerSpec extends FunSpec with Matchers {

  it ("validates indentation rules") {
    rule("strictly parse a 0-length indent")
      { Rules.▶▶▶(0) } (good = "")(bad =
        Tokens.INDENT
      )

    rule("strictly parse the default 2-space indent")
      { Rules.▶▶ } (good =
        "  "
      )( bad =
        "",
        " ",
        "   ",
        "    "
      )

    rule ("strictly parse multiple indent")
      { Rules.▶▶▶(2) } (good =
        Tokens.INDENT + Tokens.INDENT
      )( bad =
        "",
        Tokens.INDENT,
        Tokens.INDENT + Tokens.INDENT + Tokens.INDENT
      )
  }

  it ("validates cell of header or result") {
    rule ("check against different combinations")
      { Rules.Cell } ( good =
        "",
        "absd",
        "``",
        "` `",
        "`dasd asdasd\t`"
      ) ( bad =
        " hi",
        "h i",
        "hi ",
        "\t",
        " ",
        "`",
        "`sdfsd",
        "` "
      )

  }
//
//  describe ("header rules") {
//    it ("should parse a header") {
//      val header = "\thi\tthere this is\ta header"
//
//      Rules.Headers(1).parse(header) match {
//        case Parsed.Success(v, i)  => println (v)
//        case f: Parsed.Failure[_, _] => throw new Exception(f.toString())
//      }
//    }
//  }

  def rule[T](description: String)(parser: Parser[T])(good: String*)(bad: String*) {
    println(s"> Rule: $description")
    val strictRule = Start ~ parser ~ End
    println(s"\t > Validating parser: $strictRule")
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

package dataheretic.compiler

import fastparse.all._
import fastparse.core.Parsed
import org.scalatest.{FunSpec, Matchers}
import DHCompiler.Rules.General._
import DHCompiler.Rules.SQLTests._
import dataheretic.compiler.DHCompiler.AST._
import dataheretic.compiler.DHCompiler.{SQLTestsTokens, CommonTokens}

class DHCompilerSpec extends FunSpec with Matchers {

  it ("validates parsing rules") {
    rule("validate a 1-length indent") {
      ▶▶▶(1)
    }()(bad =
      "",
      " ",
      CommonTokens.INDENT + " "
    )

    rule("validate spacing rule between tokens") {
      "a" ~ ▶▶ ~ "b"
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
      LiteralCell ~ End | Start ~ QuotedCell
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

    rule("validate a given clause with 1-width indent") {
      Given(1)
    } () (bad =
      """
        |given
        |  no initial indent
      """.stripMargin.trim
    )

    rule("validate results with 1-width indent") {
      Results(1)
    } () (bad =
      """
        |this `is not` indented
        |1    2        `3 4`
      """.stripMargin.trim
    )

    // Validate indentation-sensitive rules
    for (i <- 0 to 2) {

      implicit val ii = i // Seeds the case generation in the rules.

      rule(s"validate ${i}-width indent") {
        ▶▶▶(i)
      }(good =
        ""
      )(bad =
        "" + CommonTokens.INDENT,
        "" + " "
      )

      val goodRows = Seq(
        "this",
        "`th at`",
        "something  or  `other`",
        "`even\nnewlines`  are  accepted"
      )

      val badRows = Seq(
        " plus a single space indent",
        "",
        s"${CommonTokens.INDENT}overly indented row",
        "\tha"
      )

      rule(s"validate header row with ${i}-indentation") {
        HeaderRow(i)
      }(good = goodRows:_*)(bad = badRows:_*)

      rule(s"validate result row with ${i}-indentation") {
        ResultRow(i)
      }(good = goodRows:_*)(bad = badRows:_*)

      rule(s"validate results with ${i}-indentation") {
        Results(i)
      } ( good =
        s"""this is  sparta
           |1 2  3
           |a b c""".stripMargin
      )( bad =
        "",
        s"""short header
           |1    2   3
           |a b c""".stripMargin
        ,
        s"""very very long  header
           |1    2   3
           |a b c""".stripMargin
        ,
        s"""short first row
           |1 2
           |a b c""".stripMargin
        ,
        s"""long first row
           |1 2  3 4
           |a b c""".stripMargin
      )

      rule (s"validate it-clause with ${i}-indentation") {
        It(i)
      } ( good =
        s"it was the best of times, it was the worst of times",
        s"it is"
      ) ( bad =
        s"",
        s"it",
        s" it is out of indentation",
        s"it has new\nlines",
        s"IT is uppercased"
      )

      val goodGivenWhens = Seq(
        s"""GIVENWHEN
           |${CommonTokens.INDENT}here's a line
           |${CommonTokens.INDENT}here's another line""".stripMargin
        ,
        s"""GIVENWHEN
           |${CommonTokens.INDENT}${CommonTokens.INDENT} really indented line
           |${CommonTokens.INDENT}here's another line""".stripMargin
        )

      val badGivenWhens = Seq(
        ""
        ,
        s"GIVENWHEN"
        ,
        s"GIVENWHEN\n"
        ,
        s"""GIVENWHEN
           |not a +1-indented line""".stripMargin
        ,
        s"""GIVENWHEN
           |not an indented line""".stripMargin

      )

      rule (s"validate given-clause with ${i}-indentation") {
        Given(i)
      } (good =
          goodGivenWhens map (_.replaceAll("GIVENWHEN", "given")): _*) (bad =
          badGivenWhens  map (_.replaceAll("GIVENWHEN", "given")): _*
        )

      rule (s"validate when-clause with ${i}-indentation") {
        When(i)
      } (good =
        goodGivenWhens map (_.replaceAll("GIVENWHEN", "when")): _*) (bad =
        badGivenWhens  map (_.replaceAll("GIVENWHEN", "when")): _*
      )

      val goodResults = Seq (
        s"""this `is not` sparta
           |1    2        `3 4`
           |a b    c""".stripMargin
        ,
        s"this `is just` a header"
      )
      val badResults = Seq (
        ""
        ,
        s"""  this `is not` indented correctly
           |1    2        3        4""".stripMargin
        ,
        s"""this `is not` indented correctly
           |  1    2        3        4""".stripMargin
      )

      rule (s"validate results with ${i}-indentation") {
        Results(i)
      } (good = goodResults:_*) (bad = badResults:_*)

      rule (s"validate then-clause with ${i}-indentation") {
        Then(i)
      } (good =
        s"""then
           |${goodResults(0) |>> 1 }""".stripMargin
      ) (bad =
        ""
        ,
        s"""then
           |${badResults(0) |>> 1 }""".stripMargin
        ,
        s"""then
           |${goodResults(0)}""".stripMargin
        ,
        "then"
        ,
        s"""${CommonTokens.INDENT}then
           |${goodResults(0) |>> 1 }""".stripMargin
      )

      rule (s"validate test case with ${i}-indentation") {
        TestClause(i)
      } (good =
        """it should test our clever assumptions
          |given
          |  this thing right there
          |when
          |  i do this really clever thing
          |then
          |  col1 col2 col3
          |  1    2    3
          |  a    b    c""".stripMargin
        ,
        """it should work without a given
          |when
          |  i do this really clever thing
          |then
          |  col1 col2 col3
          |  1    2    3
          |  a    b    c""".stripMargin
      ) (bad =
        ""
        ,
        """it should not run without a when
          |given
          |  this thing right there
          |then
          |  col1 col2 col3
          |  1    2    3
          |  a    b    c""".stripMargin
        ,
        """it must have a then
          |given
          |  this thing right there
          |when
          |  i do this really clever thing""".stripMargin
        ,
        """given
          |  must have an it clause!
          |when
          |  i do this really clever thing
          |then
          |  col1 col2 col3
          |  1    2    3
          |  a    b    c""".stripMargin
      )

      rule(s"validate an ensuring clause with ${i}-indentation") {
        Ensuring(i)
      } (good =
        """ENSURING
          |  it should test our clever assumptions
          |  given
          |    this thing right there
          |  when
          |    i do this really clever thing
          |  then
          |    col1 col2 col3
          |    1    2    3
          |    a    b    c
          |
          |  it should work without a given
          |  when
          |    i do this really clever thing
          |  then
          |    col1 col2 col3
          |    1    2    3
          |    a    b    c""".stripMargin
      )()
    }
  }

  it ("validates the Ensuring results") {
    val ensuring = Ensuring(0).parse(
      """ENSURING
        |  it should test our clever assumptions
        |  given
        |    this thing right there
        |    and that one, too
        |  when
        |    i do this really clever thing
        |  then
        |    col1 col2  `col 3`
        |    1    `2 3`  4
        |    a    b      c
        |
        |  it should work without a given
        |  when
        |    i do this really clever thing
        |  then
        |    col1 col2 col3
        |    1    2    3
        |    a    b    c""".stripMargin).get.value

    ensuring shouldBe Seq(
      TestCase(
        it = "should test our clever assumptions",
        given = Some("this thing right there\nand that one, too"),
        when = "i do this really clever thing",
        `then` = Seq(
          Seq("col1" -> "1", "col2" -> "2 3", "col 3" -> "4"),
          Seq("col1" -> "a", "col2" -> "b", "col 3" -> "c")
        )),
      TestCase(
        it = "should work without a given",
        given = None,
        when = "i do this really clever thing",
        `then` = Seq(
          Seq("col1" -> "1", "col2" -> "2", "col3" -> "3"),
          Seq("col1" -> "a", "col2" -> "b", "col3" -> "c")
        ))
    )
  }


  def rule[T](description: String)(parser: Parser[T])(good: String*)(bad: String*)(implicit indent: Int = 0) {
    println(s"> Rule: $description")
    val strictRule = Start ~ parser ~ End
    println(s"\t > With parser: $strictRule")
    good foreach { s =>
      val indentedCase = s |>> indent
      println(s"\t\t> Good:\t'$indentedCase'")
      strictRule.parse(indentedCase) match {
        case _: Parsed.Success[T, _, _] =>
        case f: Parsed.Failure[T, _] =>
          println (s"Unexpectedly fail on '$indentedCase'")
          println (f)
          throw new IllegalArgumentException(f.msg)
      }
    }

    bad foreach { s =>
      val indentedCase = s |>> indent
      println(s"\t\t> Bad:\t'$indentedCase'")
      strictRule.parse(indentedCase) match {
        case _: Parsed.Failure[T, _] => // good!
        case _ => throw new IllegalArgumentException(s"Unexpectedly succeed on '$indentedCase'")
      }
    }
  }

  implicit class StringExt (s: String) {

    /**
      * Indents a (multiline) string `indent`-times using Tokens.INDENT
      */
    def |>> (indent: Int): String = {
      val in = (0 until indent).foldLeft("") { (acc, _) => acc + CommonTokens.INDENT }
      in + s.replaceAll("\n", "\n" + in)
    }
  }

}

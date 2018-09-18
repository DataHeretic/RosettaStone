package dataheretic.compiler

import fastparse.all._
import fastparse.core.Parsed
import org.scalatest.{FunSpec, Matchers}
import DHCompiler.Rules.General._
import DHCompiler.Rules.SQLTests._
import dataheretic.compiler.DHCompiler.AST._
import dataheretic.compiler.DHCompiler.Rules.Migrations
import dataheretic.compiler.DHCompiler.{AST, CommonTokens}

class DHCompilerSpec extends FunSpec with Matchers {

  it ("validates indent rules") {
    rule("validate a 1-length indent") {
      ▶▶▶(1)
    }()(bad =
      "",
      " ",
      CommonTokens.INDENT + " "
    )

    for (i <- 0 to 2) {
      rule(s"validate ${i}-width indent") {
        ▶▶▶(i)
      }(good =
        ""
      )(bad =
        "" + CommonTokens.INDENT,
        "" + " "
      )(i) // Seeds the indentation of the cases
    }
  }

  it ("validates spacing rules between tokens") {
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
  }

  it ("validates cell rules") {
    rule("validate cell rules") {
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
  }

  it ("validates row rules") {
    for (i <- 0 to 2) {
      implicit val ii = i // Seeds the case generation in the rules.

      rule(s"validate header row with ${i}-indentation") {
        HeaderRow(i)
      }(good = fixtures.goodRows:_*)(bad = fixtures.badRows:_*)

      rule(s"validate result row with ${i}-indentation") {
        ResultRow(i)
      }(good = fixtures.goodRows:_*)(bad = fixtures.badRows:_*)

    }
  }

  it ("validates the results-clause rules") {
    rule("validate results with 1-width indent") {
      Results(1)
    } () (bad =
      """
        |this `is not` indented
        |1    2        `3 4`
      """.stripMargin.trim
    )

    for (i <- 0 to 2) {
      implicit val ii = i // Seeds the case generation in the rules.

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
    }
  }

  it ("validates it-clause rules") {
    for (i <- 0 to 2) {
      implicit val ii = i // Seeds the case generation in the rules.

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
    }
  }

  it ("validates given-clause parsing rules") {
    rule("validate a given clause with 1-width indent") {
      Given(1)
    } () (bad =
      """
        |given
        |  no initial indent
      """.stripMargin.trim
    )

    for (i <- 0 to 2) {
      implicit val ii = i // Seeds the case generation in the rules.
      rule (s"validate given-clause with ${i}-indentation") {
        Given(i)
      } (good =
        fixtures.goodGivenWhens map (_.replaceAll("GIVENWHEN", "given")): _*) (bad =
        fixtures.badGivenWhens  map (_.replaceAll("GIVENWHEN", "given")): _*
      )
    }
  }

  it ("validates when-clause parsing rules") {
    rule("validate a when clause with 1-width indent") {
      When(1)
    } () (bad =
      """
        |when
        |  no initial indent
      """.stripMargin.trim
    )

    for (i <- 0 to 2) {
      implicit val ii = i // Seeds the case generation in the rules.
      rule (s"validate when-clause with ${i}-indentation") {
        When(i)
      } (good =
        fixtures.goodGivenWhens map (_.replaceAll("GIVENWHEN", "when")): _*) (bad =
        fixtures.badGivenWhens  map (_.replaceAll("GIVENWHEN", "when")): _*
      )
    }
  }

  it ("validates the test clause") {
    for (i <- 0 to 2) {
      implicit val ii = i // Seeds the case generation in the rules.
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
    }
  }

  it ("validates the ensuring clause rules") {
    for (i <- 0 to 2) {
      implicit val ii = i

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

  it ("validates Migrations.Version rules") {
    rule ("validates Migrations.Version") {
      Migrations.Version
    } ( good =
      "(1)", "(123)", "(0)"
    ) (bad =
      "", "()", "( 1)", "(1 )", "( )", "(1 1)",
      "(1.0)", "(1a)", "(a1)"
    )
  }

  it ("validates Migrations.Meta rules") {
    rule("validates Migration.Meta") {
      Migrations.Meta
    } (good =
      "MIGRATION(1) no space between MIGRATION and version",
      "MIGRATION (1)  some space given"
    ) (bad =
      "MIGRATION(1)nospace given from version to description",
      "MIGRATION(1)", // no description
      "MIGRATION no version parens"
    )
  }

  it ("validates a SQL Migration script") {
    val migration = Migrations.Migration.parse(
      """MIGRATION(1) setting up some tables
        |UP
        |  CREATE TABLE categories (
        |    id    serial      PRIMARY KEY,
        |    name  varchar(32) not null
        |  );
        |DOWN
        |  drop table categories;
        |
        |ENSURING
        |  it should test our clever assumptions
        |  given
        |    this thing right there
        |    and that one, too
        |  when
        |    i do this really clever thing
        |  then
        |    col1 col2  `col 3`
        |    1    `2 3`  4
        |    a    b      c""".stripMargin).get.value

    migration shouldBe AST.Migration(
      version = 1,
      description = "setting up some tables",
      upSQL =
        """CREATE TABLE categories (
          |  id    serial      PRIMARY KEY,
          |  name  varchar(32) not null
          |);""".stripMargin,
      downSQL = "drop table categories;",
      tests = Some(Seq(
        TestCase(
          it = "should test our clever assumptions",
          given = Some("this thing right there\nand that one, too"),
          when = "i do this really clever thing",
          `then` = Seq(
            Seq("col1" -> "1", "col2" -> "2 3", "col 3" -> "4"),
            Seq("col1" -> "a", "col2" -> "b", "col 3" -> "c")
          ))))
    )
  }


  val fixtures = new {
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
  }

  def rule[T](description: String)(parser: Parser[T])(good: String*)(bad: String*)(implicit indent: Int = 0) {
    println(s"> Rule: $description")
    val strictRule = Start ~ parser ~ End
    println(s"\t > With parser: $strictRule")
    good foreach { s =>
      val indentedCase = indent |>>: s
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
      val indentedCase = indent |>>: s
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
    def |>>:(indent: Int): String = {
      val in = (0 until indent).foldLeft("") { (acc, _) => acc + CommonTokens.INDENT }
      in + s.replaceAll("\n", "\n" + in)
    }
  }
}

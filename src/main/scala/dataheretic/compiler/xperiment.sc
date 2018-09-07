import dataheretic.compiler.DHCompiler._
import fastparse.all._

(Start ~ Rules.Cell ~ End).parse("")



(Start ~ Rules.Cell ~ End).parse("`hey`") // Failure
(Start ~ (Rules.QuotedCell  | Rules.LiteralCell) ~ End).parse("`hey`") // Success
(Start ~ (Rules.LiteralCell ~ End | Start ~ Rules.QuotedCell) ~ End).parse("`hey`") // Failure
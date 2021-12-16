package replcalc.expressions

import replcalc.Parser

object Failure extends Parseable[Expression]:
  override def parse(parser: Parser, line: String): ParsedExpr[Expression] = 
    ParsedExpr.error(s"Unable to parse: $line")


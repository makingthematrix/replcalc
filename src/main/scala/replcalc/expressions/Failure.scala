package replcalc.expressions

import replcalc.Parser

object Failure extends Parseable[Expression]:
  override def parse(line: String, parser: Parser): ParsedExpr[Expression] = 
    ParsedExpr.error(s"Unable to parse: $line")


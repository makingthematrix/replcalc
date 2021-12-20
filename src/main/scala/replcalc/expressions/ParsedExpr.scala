package replcalc.expressions

import Error.ParsingError

type ParsedExpr[T <: Expression] = Option[Either[Error, T]]

extension [T <: Expression] (parsedExpr: ParsedExpr[T])
  def happyPath[S <: Expression](f: T => ParsedExpr[S]): ParsedExpr[S] =
    parsedExpr.flatMap {
      case Left(error)       => ParsedExpr.error(error)
      case Right(expression) => f(expression)
    }

  def errorIfEmpty(error: Error): ParsedExpr[T] = parsedExpr.orElse(ParsedExpr.error(error))
  def errorIfEmpty(errorMsg: String): ParsedExpr[T] = parsedExpr.orElse(ParsedExpr.error(errorMsg))

object ParsedExpr:
  def apply[T <: Expression](expr: T): ParsedExpr[T] = Some(Right(expr))
  def error[T <: Expression](error: Error): ParsedExpr[T] = Some(Left(error))
  def error[T <: Expression](errorMsg: String): ParsedExpr[T] = Some(Left(ParsingError(errorMsg)))
  def empty[T <: Expression]: ParsedExpr[T] = Option.empty[Either[Error, T]]
  def unused[T <: Expression]: ParsedExpr[T] = Some(Left(ParsingError("Unused expression")))

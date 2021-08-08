package org.compiler.example
package parser.expr

import parser.expr.ParserExpr.{ExprResult, ParserExpr, advance, unit}

object ParserLiteral {

  def parserLiteral[A](value: A): ParserExpr = advance(literal(value))

  def literal[A](value: A): ParserExpr = unit(createLiteral(value))

  private def createLiteral[A](value: A): ExprResult = Right(Literal(value))
}

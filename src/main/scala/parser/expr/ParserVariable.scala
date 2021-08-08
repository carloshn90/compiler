package org.compiler.example
package parser.expr

import lexer.Token
import parser.expr.ParserExpr.{ExprResult, ParserExpr, advance, unit}

object ParserVariable {

  def parserVariable(token: Token): ParserExpr = advance(variable(token))

  private def variable(token: Token): ParserExpr = unit(createVariable(token))

  private def createVariable(token: Token): ExprResult = Right(Variable(token))
}

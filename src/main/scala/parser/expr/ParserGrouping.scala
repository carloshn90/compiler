package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{RIGHT_PAREN, Token}
import parser.expr.ParserExpr.{ExprResult, ParserExpr, advance, flatMap, unit}

object ParserGrouping {

  def parserGrouping()(f: () => ParserExpr): ParserExpr = {
    lazy val rightParserExpr = advance(f())
    flatMap(rightParserExpr)((left, tokenList) => grouping(left, tokenList.head))
  }

  def grouping(expr: ExprResult, token: Token): ParserExpr = {
    if (token.tokenType == RIGHT_PAREN) {
      lazy val createGroupingExprResult = unit(createGrouping(expr))
      advance(createGroupingExprResult)
    } else unit(Left(ErrorCompiler(token.line, s"Expect ')' after expression '${token.lexeme}'")))
  }

  private def createGrouping(expr: ExprResult): ExprResult = expr.map(e => Grouping(e))
}

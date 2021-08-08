package org.compiler.example
package parser.expr

import lexer.Token.matchType
import lexer.{Token, TokenType}
import parser.expr.ParserExpr.{ExprResult, ParserExpr, advance, flatMap, unit}

object ParserUnary {

  def parserUnary(types: Seq[TokenType])(f: () => ParserExpr, g: () => ParserExpr): ParserExpr =
    tokenList => unary(types, tokenList.head)(f, g)(tokenList)

  private def unary(types: Seq[TokenType], token: Token)(f: () => ParserExpr, g: () => ParserExpr): ParserExpr = {
    if (matchType(types, token)) {
      lazy val rightParserExpr = advance(f())
      flatMap(rightParserExpr)((right, _) => createUnary(right, token))
    } else g()
  }

  private def createUnary(right: ExprResult, token: Token): ParserExpr =
    unit(right.map(r => Unary(token, r)))
}

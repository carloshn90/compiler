package org.compiler.example
package parser.expr

import lexer.Token.matchType
import lexer.{Token, TokenType}
import parser.expr.ParserExpr.{ExprResult, ParserExpr, advance, flatMap, map2, unit}

object ParserBinary {

  def parserBinary(parserExpr: ParserExpr, types: Seq[TokenType])(f: () => ParserExpr): ParserExpr =
    flatMap(parserExpr)((left, leftTokenList) => binary(left, types, leftTokenList.head)(f))

  private def binary(left: ExprResult, types: Seq[TokenType], token: Token)(f: () => ParserExpr): ParserExpr = {
    if (matchType(types, token)) {
      lazy val rightParserExpr = advance(f())
      flatMap(rightParserExpr)((right, tokens) => binary(createBinary(left, right, token), types, tokens.head)(f))
    } else unit(left)
  }

  private def createBinary(left: ExprResult, right: ExprResult, token: Token): ExprResult =
    map2(left, right)((l, r) => Binary(l, token, r))
}

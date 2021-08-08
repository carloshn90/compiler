package org.compiler.example
package parser.expr

import lexer.Token.matchType
import lexer.{Token, TokenType}
import parser.grammar.GrammarResult.{GrammarResult, ParserTypeResultMonad}
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, advance, unit}

object ParserBinary {

  def parserBinary(parserExpr: ParserGrammar[Expr], types: Seq[TokenType])(f: () => ParserGrammar[Expr]): ParserGrammar[Expr] =
    parserExpr.flatMap((left, leftTokenList) => binary(left, types, leftTokenList.head)(f))

  private def binary(left: GrammarResult[Expr], types: Seq[TokenType], token: Token)(f: () => ParserGrammar[Expr]): ParserGrammar[Expr] = {
    if (matchType(types, token)) {
      lazy val rightParserExpr = advance(f())
      rightParserExpr.flatMap((right, tokens) => binary(createBinary(left, right, token), types, tokens.head)(f))
    } else unit(left)
  }

  private def createBinary(left: GrammarResult[Expr], right: GrammarResult[Expr], token: Token): GrammarResult[Expr] =
    left.map2(right)((l, r) => Binary(l, token, r))
}

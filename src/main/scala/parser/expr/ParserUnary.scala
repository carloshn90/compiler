package org.compiler.example
package parser.expr

import lexer.Token.matchType
import lexer.{Token, TokenType}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, advance, unit}

object ParserUnary {

  def parserUnary(types: Seq[TokenType])(f: () => ParserGrammar[Expr], g: () => ParserGrammar[Expr]): ParserGrammar[Expr] =
    tokenList => unary(types, tokenList.head)(f, g)(tokenList)

  private def unary(types: Seq[TokenType], token: Token)(f: () => ParserGrammar[Expr], g: () => ParserGrammar[Expr]): ParserGrammar[Expr] = {
    if (matchType(types, token)) {
      lazy val rightParserExpr = advance(f())
      rightParserExpr.flatMap((right, _) => createUnary(right, token))
    } else g()
  }

  private def createUnary(right: GrammarResult[Expr], token: Token): ParserGrammar[Expr] =
    unit(right.map(r => Unary(token, r)))
}

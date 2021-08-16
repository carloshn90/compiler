package org.compiler.example
package parser.expr

import lexer.{Token, TokenType}
import parser.grammar.GrammarResult.{GrammarResult, ParserTypeResultMonad}
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserLogical {

  def parserLogical(parserGrammar: ParserGrammar[Expr], tokenType: TokenType): ParserGrammar[Expr] =
    parserGrammar.flatMap((expr, _) => logical(parserGrammar, tokenType, expr))

  private def logical(parserGrammar: ParserGrammar[Expr], tokenType: TokenType, expr: GrammarResult[Expr]): ParserGrammar[Expr] = tokenList => {
    val token = tokenList.head
    if (token.tokenType == tokenType)
      parserGrammar.flatMap((right, _) => logical(parserGrammar, tokenType, createLogical(expr, token, right)))(tokenList.tail)
    else unit(expr)(tokenList)
  }

  private def createLogical(expr: GrammarResult[Expr], token: Token, right: GrammarResult[Expr]): GrammarResult[Expr] =
    expr.map2(right)((e, r) => Logical(e, token, r))
}

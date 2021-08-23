package org.compiler.example
package parser.stmt

import lexer.{EOF, SEMICOLON, Token}
import parser.expr.Expr
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserReturn {

  def parserReturn(expression: ParserGrammar[Expr]): ParserGrammar[Stmt] = tokenList => {
    val returnToken: Token = tokenList.head
    parserReturnExpression(expression)
      .flatMap(expr => createReturn(returnToken, expr))
      .consume(SEMICOLON, "Expect ';' after return value.")(tokenList.tail)
  }

  private def parserReturnExpression(expression: ParserGrammar[Expr]): ParserGrammar[Expr] = tokenList => {
    val token: Token = tokenList.head
    if (token.tokenType == SEMICOLON || token.tokenType == EOF) unit(Right(Expr.None))(tokenList)
    else expression(tokenList)
  }

  private def createReturn(token: Token, value: GrammarResult[Expr]): ParserGrammar[Stmt] =
    unit(value.map(v => Return(token, v)))
}

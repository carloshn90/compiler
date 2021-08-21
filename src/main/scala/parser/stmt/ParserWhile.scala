package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{LEFT_PAREN, RIGHT_PAREN}
import parser.expr.Expr
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserWhile {

  def parserWhileStmt(expression: ParserGrammar[Expr], statement: ParserGrammar[Stmt]): ParserGrammar[Stmt] =
    parserCondition(expression)
    .flatMap(cond => parserBody(statement).map(body => createWhile(cond, body)))

  private def parserCondition(expression: ParserGrammar[Expr]): ParserGrammar[Expr] = tokenList => {
    val token = tokenList.head
    if (token.tokenType != LEFT_PAREN) unit(Left(ErrorCompiler(token.line, "Expect '(' after 'while'.")))(tokenList)
    else expression(tokenList.tail)
  }

  private def parserBody(statement: ParserGrammar[Stmt]): ParserGrammar[Stmt] = tokenList => {
    val token = tokenList.head
    if (token.tokenType != RIGHT_PAREN) unit(Left(ErrorCompiler(token.line, "Expect ')' after 'condition'.")))(tokenList)
    else statement(tokenList.tail)
  }

  private def createWhile(condition: GrammarResult[Expr], body: GrammarResult[Stmt]): GrammarResult[Stmt] = for {
    c <- condition
    b <- body
  } yield While(c, b)
}

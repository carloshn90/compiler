package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{ELSE, LEFT_PAREN, RIGHT_PAREN}
import parser.expr.Expr
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserIf {

  def parserIf(expression: ParserGrammar[Expr], statement: ParserGrammar[Stmt]): ParserGrammar[Stmt] =
    parserCondition(expression)
      .flatMap2(parserThenBranch(statement))((cond, thenB) => parserElseBranch(cond, thenB, statement))

  private def parserCondition(expression: ParserGrammar[Expr]): ParserGrammar[Expr] = tokenList => {
    val token = tokenList.head
    if (token.tokenType != LEFT_PAREN) unit(Left(ErrorCompiler(token.line, "Expect '(' after 'if'.")))(tokenList)
    else expression(tokenList.tail)
  }

  private def parserThenBranch(statement: ParserGrammar[Stmt]): ParserGrammar[Stmt] = tokenList => {
    val token = tokenList.head
    if (token.tokenType != RIGHT_PAREN) unit(Left(ErrorCompiler(token.line, "Expect ')' after if condition.")))(tokenList)
    else statement(tokenList.tail)
  }

  private def parserElseBranch(cond: GrammarResult[Expr], thenB: GrammarResult[Stmt], statement: ParserGrammar[Stmt]): ParserGrammar[Stmt] = tokenList => {
    val token = tokenList.head
    if (token.tokenType == ELSE) statement.map((elseB, _) => createIf(cond, thenB, elseB))(tokenList.tail)
    else (createIf(cond, thenB), tokenList)
  }

  private def createIf(condition: GrammarResult[Expr], thenBranch: GrammarResult[Stmt]): GrammarResult[Stmt] = for {
    c <- condition
    t <- thenBranch
  } yield If(c, t, None)

  private def createIf(condition: GrammarResult[Expr], thenBranch: GrammarResult[Stmt], elseBranch: GrammarResult[Stmt]): GrammarResult[Stmt] = for {
    c <- condition
    t <- thenBranch
    e <- elseBranch
  } yield If(c, t, Some(e))
}

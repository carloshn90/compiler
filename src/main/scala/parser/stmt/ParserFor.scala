package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, LEFT_PAREN, RIGHT_PAREN, SEMICOLON, Token, VAR}
import parser.expr.{Expr, Literal}
import parser.grammar.GrammarResult.{GrammarResult, ParserTypeResultMonad}
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserFor {

  def parserFor(varDecl: ParserGrammar[Stmt], exprStmt: ParserGrammar[Stmt],
                expression: ParserGrammar[Expr], statement: ParserGrammar[Stmt]): ParserGrammar[Stmt] = for {
    init <- parserInitializer(varDecl, exprStmt)
    cond <- parserCondition(expression)
    incr <- parserIncrement(expression)
    body <- statement
  } yield createFor(init, cond, incr, body)

  private def parserInitializer(varDecl: ParserGrammar[Stmt], exprStmt: ParserGrammar[Stmt]): ParserGrammar[Stmt] = tokenList => {
    val token = tokenList.head
    if (token.tokenType != LEFT_PAREN) unit(Left(ErrorCompiler(token.line, "Expect '(' after 'for'.")))(tokenList)
    else createInitializer(varDecl, exprStmt)(tokenList.tail)
  }

  private def createInitializer(varDecl: ParserGrammar[Stmt], exprStmt: ParserGrammar[Stmt]): ParserGrammar[Stmt] = {
    case Token(SEMICOLON, _, _, _)::tail  =>  unit(Right(Stmt.None))(tail)
    case Token(VAR, _, _, _)::tail        =>  varDecl(tail)
    case tokenList@_::_                   =>  exprStmt(tokenList)
  }

  private def parserCondition(expression: ParserGrammar[Expr]): ParserGrammar[Expr] = {
    case tokenList@Token(EOF, _, line, _)::_  => unit(Left(ErrorCompiler(line, "Malformed for expression")))(tokenList)
    case Token(SEMICOLON, _, _, _)::tail      => unit(Right(Literal(true)))(tail)
    case tokenList@_::_                       => expression.consume(SEMICOLON, "Expect ';' after loop condition.")(tokenList)
  }

  private def parserIncrement(expression: ParserGrammar[Expr]): ParserGrammar[Expr] = {
    case tokenList@Token(EOF, _, line, _)::_  => unit(Left(ErrorCompiler(line, "Malformed for expression")))(tokenList)
    case Token(RIGHT_PAREN, _, _, _)::tail    => unit(Right(Expr.None))(tail)
    case tokenList@_::_                       => expression.consume(RIGHT_PAREN, "Expect ')' after for clauses.")(tokenList)
  }

  private def createFor(init: GrammarResult[Stmt], cond: GrammarResult[Expr],
                        incr: GrammarResult[Expr], body: GrammarResult[Stmt]): GrammarResult[Stmt] = {
    init.flatMap2(cond)((i, c) => incr.map2(body)((ii, b) => addInitializer(i, addCondition(c, addIncrement(ii, b)))))
  }

  private def addInitializer(init: Stmt, body: Stmt): Stmt = init match {
    case Stmt.None  => body
    case _          => Block(List(init, body))
  }

  private def addCondition(cond: Expr, body: Stmt): Stmt = cond match {
    case Expr.None => Expression(Literal(true))
    case _         => While(cond, body)
  }

  private def addIncrement(incr: Expr, body: Stmt): Stmt = incr match {
    case Expr.None => body
    case _         => Block(List(body, Expression(incr)))
  }
}

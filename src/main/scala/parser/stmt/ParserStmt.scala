package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.Token
import parser.expr.ParserExpr.{ExprResult, ParserExpr}

object ParserStmt {
  type ParserResult = List[Token] => Either[ErrorCompiler, List[Stmt]]
  type StmtResult = Either[ErrorCompiler, Stmt]
  type ParserStmt = List[Token] => (StmtResult, List[Token])

  def unit(stmt: StmtResult): ParserStmt = tokenList => (stmt, tokenList)

  def map(parserExpr: ParserExpr)(f: (ExprResult, List[Token]) => StmtResult): ParserStmt =
    flatMap(parserExpr)((expr, token) => unit(f(expr, token)))

  def flatMap(parserExpr: ParserExpr)(f: (ExprResult, List[Token]) => ParserStmt): ParserStmt = tokenList => {
    parserExpr(tokenList) match {
      case (Right(expr), tokens) => f(Right(expr), tokens)(tokens)
      case (Left(err), tokens)   => (Left(err), tokens)
    }
  }

  def mapStmt(parserExpr: ParserStmt)(f: (StmtResult, List[Token]) => StmtResult): ParserStmt =
    flatMapStmt(parserExpr)((expr, token) => unit(f(expr, token)))

  def flatMapStmt(parserExpr: ParserStmt)(f: (StmtResult, List[Token]) => ParserStmt): ParserStmt = tokenList => {
    val (expr: StmtResult, nextTokenList: List[Token]) = parserExpr(tokenList)
    f(expr, nextTokenList)(nextTokenList)
  }

  def advance(parserExpr: ParserStmt): ParserStmt = tokenList => {
    val (expr, newTokenList) = parserExpr(tokenList.tail)
    unit(expr)(newTokenList)
  }

  def createSemicolonError(line: Int): StmtResult =
    Left(ErrorCompiler(line, "Expect ';' after value."))
}

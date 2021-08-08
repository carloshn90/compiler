package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EQUAL, IDENTIFIER, SEMICOLON, Token}
import parser.expr.ParserExpr.{ExprResult, ParserExpr}
import parser.stmt.ParserStmt.{ParserStmt, StmtResult, createSemicolonError, map, flatMapStmt, unit}

object ParserVar {

  def parserVar(parserExpr: ParserExpr): ParserStmt = {
    case tokenList@Token(IDENTIFIER, _, _, _)::tail  => variable(parserExpr, tokenList.head)(tail)
    case tokenList@Token(_, _, line, _)::_           => unit(Left(ErrorCompiler(line, "Expect variable name.")))(tokenList)
  }

  private def variable(parserExpr: ParserExpr, identifier: Token): ParserStmt =
    flatMapStmt(initializerVar(parserExpr, identifier))((expr, _) => checkSemicolon(expr))

  private def initializerVar(parserExpr: ParserExpr, identifier: Token): ParserStmt = {
    case Token(EQUAL, _, _, _)::tail  => map(parserExpr)((left, _) => createVar(left, identifier))(tail)
    case tokenList@_                  => unit(createVar(Right(null), identifier))(tokenList)
  }

  private def createVar(left: ExprResult, identifier: Token): StmtResult =
    left.map(l => Var(identifier, l))

  private def checkSemicolon(left: StmtResult): ParserStmt = {
    case Token(SEMICOLON, _, _, _)::tail    => unit(left)(tail)
    case tokenList@Token(_, _, line, _)::_  => unit(createSemicolonError(line))(tokenList)
  }
}

package org.compiler.example
package parser.stmt

import lexer.{SEMICOLON, Token}
import parser.expr.ParserExpr.{ExprResult, ParserExpr}
import parser.stmt.ParserStmt.{ParserStmt, StmtResult, createSemicolonError, flatMap, unit}

object ParserPrint {

  def parserPrint(parserExpr: ParserExpr): ParserStmt =
    flatMap(parserExpr)((left, _) => print(left))

  private def print(left: ExprResult): ParserStmt = {
    case tokenList@Token(SEMICOLON, _, _, _)::_ => unit(createPrint(left))(tokenList.tail)
    case tokenList@Token(_, _, line, _)::_      => unit(createSemicolonError(line))(tokenList)
  }

  private def createPrint(left: ExprResult): StmtResult =
    left.map(l => Print(l))
}

package org.compiler.example
package parser.stmt

import lexer.{SEMICOLON, Token}
import parser.expr.ParserExpr.{ExprResult, ParserExpr}
import parser.stmt.ParserStmt.{ParserStmt, StmtResult, createSemicolonError, flatMap, unit}

object ParserExpression {

  def parserExpression(parserExpr: ParserExpr): ParserStmt =
    flatMap(parserExpr)((left, _) => expression(left))

  private def expression(left: ExprResult): ParserStmt = {
    case tokenList@Token(SEMICOLON, _, _, _)::_ => unit(createExpression(left))(tokenList.tail)
    case tokenList@Token(_, _, line, _)::_      => unit(createSemicolonError(line))(tokenList)
  }

  private def createExpression(left: ExprResult): StmtResult =
    left.map(expr => Expression(expr))
}

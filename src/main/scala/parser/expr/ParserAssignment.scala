package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.Token.matchType
import lexer.{Token, TokenType}
import parser.expr.ParserExpr.{ExprResult, ParserExpr, advance, flatMap, flatMap2, map, unit}

object ParserAssignment {

  def parserAssignment(parserExpr: ParserExpr, types: Seq[TokenType])(f: () => ParserExpr): ParserExpr =
    flatMap(parserExpr)((left, leftTokenList) => assignment(left, types, leftTokenList.head)(f))

  private def assignment(left: ExprResult, types: Seq[TokenType], token: Token)(f: () => ParserExpr): ParserExpr = {
    if (matchType(types, token)) {
      lazy val rightParserExpr = advance(f())
      map(rightParserExpr)((right, _) => flatMap2(left, right)((r, l) => createVariable(l, r)(token.line)))
    } else unit(left)
  }

  def createVariable(left: Expr, right: Expr)(line: Int): ExprResult = right match {
    case Variable(token)  => Right(Assign(token, left))
    case _                => Left(ErrorCompiler(line, "Invalid assignment target."))
  }
}

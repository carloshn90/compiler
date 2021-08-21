package org.compiler.example
package parser.stmt

import lexer.{SEMICOLON, Token}
import parser.expr.Expr
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}
import parser.stmt.ParserStmt.createSemicolonError

object ParserExpression {

  def parserExpression(parserExpr: ParserGrammar[Expr]): ParserGrammar[Stmt] =
    parserExpr.flatMap(left => expression(left))

  private def expression(left: GrammarResult[Expr]): ParserGrammar[Stmt] = {
    case tokenList@Token(SEMICOLON, _, _, _)::_ => unit(createExpression(left))(tokenList.tail)
    case tokenList@Token(_, _, line, _)::_      => unit(createSemicolonError(line))(tokenList)
  }

  private def createExpression(left: GrammarResult[Expr]): GrammarResult[Stmt] =
    left.map(expr => Expression(expr))
}

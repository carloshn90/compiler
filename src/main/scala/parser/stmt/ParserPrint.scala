package org.compiler.example
package parser.stmt

import lexer.{SEMICOLON, Token}
import parser.expr.Expr
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}
import parser.stmt.ParserStmt.createSemicolonError

object ParserPrint {

  def parserPrint(parserExpr: ParserGrammar[Expr]): ParserGrammar[Stmt] =
    parserExpr.flatMap((left, _) => print(left))

  private def print(left: GrammarResult[Expr]): ParserGrammar[Stmt] = {
    case tokenList@Token(SEMICOLON, _, _, _)::_ => unit(createPrint(left))(tokenList.tail)
    case tokenList@Token(_, _, line, _)::_      => unit(createSemicolonError(line))(tokenList)
  }

  private def createPrint(left: GrammarResult[Expr]): GrammarResult[Stmt] =
    left.map(l => Print(l))
}

package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EQUAL, IDENTIFIER, SEMICOLON, Token}
import parser.expr.{Expr, Literal}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}
import parser.stmt.ParserStmt.createSemicolonError

object ParserVar {

  def parserVar(parserExpr: ParserGrammar[Expr]): ParserGrammar[Stmt] = {
    case tokenList@Token(IDENTIFIER, _, _, _)::tail  => variable(parserExpr, tokenList.head)(tail)
    case tokenList@Token(_, _, line, _)::_           => unit(Left(ErrorCompiler(line, "Expect variable name.")))(tokenList)
  }

  private def variable(parserExpr: ParserGrammar[Expr], identifier: Token): ParserGrammar[Stmt] =
    initializerVar(parserExpr, identifier).flatMap(checkSemicolon)

  private def initializerVar(parserExpr: ParserGrammar[Expr], identifier: Token): ParserGrammar[Stmt] = {
    case Token(EQUAL, _, _, _)::tail  => parserExpr.map(left => createVar(left, identifier))(tail)
    case tokenList@_                  => unit(createVar(Right(Literal(Nil)), identifier))(tokenList)
  }

  private def createVar(left: GrammarResult[Expr], identifier: Token): GrammarResult[Stmt] =
    left.map(l => Var(identifier, l))

  private def checkSemicolon(left: GrammarResult[Stmt]): ParserGrammar[Stmt] = {
    case Token(SEMICOLON, _, _, _)::tail    => unit(left)(tail)
    case tokenList@Token(_, _, line, _)::_  => unit(createSemicolonError(line))(tokenList)
  }
}

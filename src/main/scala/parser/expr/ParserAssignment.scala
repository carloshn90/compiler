package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.Token.matchType
import lexer.{Token, TokenType}
import parser.grammar.GrammarResult.{GrammarResult, ParserTypeResultMonad}
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, advance, unit}


object ParserAssignment {

  def parserAssignment(parserExpr: ParserGrammar[Expr], types: Seq[TokenType])(f: () => ParserGrammar[Expr]): ParserGrammar[Expr] =
    parserExpr.flatMap((left, leftTokenList) => assignment(left, types, leftTokenList.head)(f))

  private def assignment(left: GrammarResult[Expr], types: Seq[TokenType], token: Token)(f: () => ParserGrammar[Expr]): ParserGrammar[Expr] = {
    if (matchType(types, token)) {
      lazy val rightParserExpr = advance(f())
      rightParserExpr.map((right, _) => left.flatMap2(right)((r, l) => createVariable(l, r)(token.line)))
    } else unit(left)
  }

  def createVariable(left: Expr, right: Expr)(line: Int): GrammarResult[Expr] = right match {
    case Variable(token)  => Right(Assign(token, left))
    case _                => Left(ErrorCompiler(line, "Invalid assignment target."))
  }
}

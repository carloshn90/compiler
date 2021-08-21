package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.RIGHT_PAREN
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, advance, unit}

object ParserGrouping {

  def parserGrouping()(f: () => ParserGrammar[Expr]): ParserGrammar[Expr] = {
    lazy val rightParserExpr = advance(f())
    rightParserExpr.flatMap(grouping)
  }

  def grouping(expr: GrammarResult[Expr]): ParserGrammar[Expr] = tokenList => {
    val token = tokenList.head
    if (token.tokenType == RIGHT_PAREN) {
      lazy val createGroupingExprResult = unit(createGrouping(expr))
      advance(createGroupingExprResult)(tokenList)
    } else unit(Left(ErrorCompiler(token.line, s"Expect ')' after expression '${token.lexeme}'")))(tokenList)
  }

  private def createGrouping(expr: GrammarResult[Expr]): GrammarResult[Expr] = expr.map(e => Grouping(e))
}

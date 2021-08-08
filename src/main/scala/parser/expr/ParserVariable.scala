package org.compiler.example
package parser.expr

import lexer.Token
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserGrammar, advance, unit}

object ParserVariable {

  def parserVariable(token: Token): ParserGrammar[Expr] = advance(variable(token))

  private def variable(token: Token): ParserGrammar[Expr] = unit(createVariable(token))

  private def createVariable(token: Token): GrammarResult[Expr] = Right(Variable(token))
}

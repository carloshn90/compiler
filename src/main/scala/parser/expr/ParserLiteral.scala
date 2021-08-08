package org.compiler.example
package parser.expr

import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserGrammar, advance, unit}

object ParserLiteral {

  def parserLiteral[A](value: A): ParserGrammar[Expr] = advance(literal(value))

  def literal[A](value: A): ParserGrammar[Expr] = unit(createLiteral(value))

  private def createLiteral[A](value: A): GrammarResult[Expr] = Right(Literal(value))
}

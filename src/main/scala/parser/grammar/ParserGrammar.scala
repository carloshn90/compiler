package org.compiler.example
package parser.grammar

import error.ErrorCompiler
import lexer.{Token, TokenType}
import parser.grammar.GrammarResult.GrammarResult

object ParserGrammar {

  type ParserGrammar[A <: Grammar] = List[Token] => (GrammarResult[A], List[Token])

  def unit[B <: Grammar](b: GrammarResult[B]): ParserGrammar[B] = tokenList => (b, tokenList)

  def advance[B <: Grammar](parserExpr: ParserGrammar[B]): ParserGrammar[B] = tokenList => {
    val (expr, newTokenList) = parserExpr(tokenList.tail)
    unit(expr)(newTokenList)
  }

  implicit class ParserExprMonad[A <: Grammar](val parserExpr: ParserGrammar[A]) {

    def map[B <: Grammar](f: GrammarResult[A] => GrammarResult[B]): ParserGrammar[B] =
      parserExpr.flatMap(a => unit(f(a)))

    def flatMap[B <: Grammar](f: GrammarResult[A] => ParserGrammar[B]): ParserGrammar[B] = tokenList => {
      parserExpr(tokenList) match {
        case (Right(expr), tokens) => f(Right(expr))(tokens)
        case (Left(err), tokens)   => (Left(err), tokens)
      }
    }
    def map2[B <:Grammar, C <: Grammar](right: ParserGrammar[B])(f: (GrammarResult[A], GrammarResult[B]) => GrammarResult[C]): ParserGrammar[C] =
      parserExpr.flatMap(l => right.map(r => f(l, r)))

    def flatMap2[B <:Grammar, C <: Grammar](right: ParserGrammar[B])(f: (GrammarResult[A], GrammarResult[B]) => ParserGrammar[C]): ParserGrammar[C] =
      parserExpr.flatMap(l => right.flatMap(r => f(l, r)))

    def consume(tokenType: TokenType, error: String): ParserGrammar[A] = tokenList => {
      val (result, tokens) = parserExpr(tokenList)
      val token: Token = tokens.head
      result match {
        case Left(err)                                => unit(Left(err))(tokens)
        case Right(_) if token.tokenType != tokenType => unit(Left(ErrorCompiler(token.line, error)))(tokens)
        case Right(value)                             => (Right(value), tokens.tail)
      }
    }
  }
}

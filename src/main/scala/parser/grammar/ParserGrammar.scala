package org.compiler.example
package parser.grammar

import lexer.Token
import parser.grammar.GrammarResult.GrammarResult

object ParserGrammar {

  type ParserGrammar[A <: Grammar] = List[Token] => (GrammarResult[A], List[Token])

  def unit[B <: Grammar](b: GrammarResult[B]): ParserGrammar[B] = tokenList => (b, tokenList)

  def advance[B <: Grammar](parserExpr: ParserGrammar[B]): ParserGrammar[B] = tokenList => {
    val (expr, newTokenList) = parserExpr(tokenList.tail)
    unit(expr)(newTokenList)
  }

  implicit class ParserExprMonad[A <: Grammar](val parserExpr: ParserGrammar[A]) {

    def map[B <: Grammar](f: (GrammarResult[A], List[Token]) => GrammarResult[B]): ParserGrammar[B] =
      parserExpr.flatMap((a, tokens) => unit(f(a, tokens)))

    def flatMap[B <: Grammar](f: (GrammarResult[A], List[Token]) => ParserGrammar[B]): ParserGrammar[B] = tokenList => {
      parserExpr(tokenList) match {
        case (Right(expr), tokens) => f(Right(expr), tokens)(tokens)
        case (Left(err), tokens)   => (Left(err), tokens)
      }
    }
    def map2[B <:Grammar, C <: Grammar](right: ParserGrammar[B])(f: (GrammarResult[A], GrammarResult[B]) => GrammarResult[C]): ParserGrammar[C] =
      parserExpr.flatMap((l, _) => right.map((r, _) => f(l, r)))

    def flatMap2[B <:Grammar, C <: Grammar](right: ParserGrammar[B])(f: (GrammarResult[A], GrammarResult[B]) => ParserGrammar[C]): ParserGrammar[C] =
      parserExpr.flatMap((l, _) => right.flatMap((r, _) => f(l, r)))
  }
}

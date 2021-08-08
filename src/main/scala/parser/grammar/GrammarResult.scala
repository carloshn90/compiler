package org.compiler.example
package parser.grammar

import error.ErrorCompiler

object GrammarResult {

  type GrammarResult[A <: Grammar] = Either[ErrorCompiler, A]

  def unit[B <: Grammar](b: B): GrammarResult[B] = Right(b)

  implicit class ParserTypeResultMonad[A <: Grammar](val result: GrammarResult[A]) {

    def map[B <: Grammar](f: A => B): GrammarResult[B] =
      result.flatMap(a => unit(f(a)))

    def flatMap[B <: Grammar](f: A => GrammarResult[B]): GrammarResult[B] = result match {
      case Right(v)   => f(v)
      case Left(err)  => Left(err)
    }

    def map2[B <:Grammar, C <: Grammar](right: GrammarResult[B])(f: (A, B) => C): GrammarResult[C] =
      result.flatMap(l => right.map(r => f(l, r)))

    def flatMap2[B <:Grammar, C <: Grammar](right: GrammarResult[B])(f: (A, B) => GrammarResult[C]): GrammarResult[C] = (result, right) match {
      case (Right(l), Right(r)) => f(l, r)
      case (Left(err), _)       => Left(err)
      case (_, Left(err))       => Left(err)
    }
  }
}

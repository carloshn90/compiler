package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.Token

import cats.implicits.catsSyntaxApply

object ParserExpr {

  type ExprResult = Either[ErrorCompiler, Expr]
  type ParserExpr = List[Token] => (ExprResult, List[Token])

  def unit(expr: ExprResult): ParserExpr = tokenList => (expr, tokenList)

  def map(parserExpr: ParserExpr)(f: (ExprResult, List[Token]) => ExprResult): ParserExpr =
    flatMap(parserExpr)((expr, token) => unit(f(expr, token)))

  def flatMap(parserExpr: ParserExpr)(f: (ExprResult, List[Token]) => ParserExpr): ParserExpr = tokenList => {
    parserExpr(tokenList) match {
      case (Right(expr), tokens) => f(Right(expr), tokens)(tokens)
      case (Left(err), tokens)   => (Left(err), tokens)
    }
  }

  def advance(parserExpr: ParserExpr): ParserExpr = tokenList => {
    val (expr, newTokenList) = parserExpr(tokenList.tail)
    unit(expr)(newTokenList)
  }

  def map2(left: ExprResult, right: ExprResult)(f: (Expr, Expr) => Expr): ExprResult =
    left.map2(right)(f)

  def flatMap2(left: ExprResult, right: ExprResult)(f: (Expr, Expr) => ExprResult): ExprResult = (left, right) match {
    case (Right(l), Right(r)) => f(l, r)
    case (Left(err), _)       => Left(err)
    case (_, Left(err))       => Left(err)
  }
}

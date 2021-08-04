package org.compiler.example
package parser

import lexer.{EOF, RIGHT_PAREN, Token, TokenType}

import error.ErrorCompiler

import scala.annotation.tailrec
import cats.implicits._

object ParserExpression {

  type ParserExpr = List[Token] => (Either[ErrorCompiler, Expr], List[Token])

  def unit(expr: Either[ErrorCompiler, Expr]): ParserExpr = tokenList => (expr, tokenList)

  def parserBinary(parserExpr: ParserExpr, types: Seq[TokenType])(f: () => ParserExpr): ParserExpr = tokenList => {

    val (left, leftTokenList) = parserExpr(tokenList)
    val currentToken = leftTokenList.head

    if (matchType(types, currentToken)) {
      val (right, rightTokenList) = f()(leftTokenList.tail)
      val expr: Either[ErrorCompiler, Expr] = left.map2(right)((l, r) => Binary(l, currentToken, r))
      parserBinary(unit(expr), types)(f)(rightTokenList)
    } else parserExpr(tokenList)
  }

  def parserUnary(types: Seq[TokenType])(f: () => ParserExpr, g: () => ParserExpr): ParserExpr = tokenList => {

    val currentToken = tokenList.head

    if (matchType(types, currentToken)) {
      val (right, rightTokenList) = f()(tokenList.tail)
      val expr: Either[ErrorCompiler, Expr] = right.map(r => Unary(tokenList.head, r))
      unit(expr)(rightTokenList)
    } else g()(tokenList)
  }

  def parserGrouping()(f: () => ParserExpr): ParserExpr = tokenList => {

    val (expr, exprTokenList) = f()(tokenList.tail)

    if (exprTokenList.head.tokenType == RIGHT_PAREN) unit(expr.map(e => Grouping(e)))(exprTokenList.tail)
    else unit(Left(ErrorCompiler(tokenList.head.line, s"Expect ')' after expression '${tokenList.head.lexeme}''")))(exprTokenList)
  }

  def parserLiteral[A](value: A): ParserExpr = tokenList =>
    unit(Right(Literal(value)))(tokenList.tail)

  def parserVariable(): ParserExpr = tokenList =>
    unit(Right(Variable(tokenList.head)))(tokenList.tail)

  @tailrec
  private def matchType(types: Seq[TokenType], token: Token): Boolean = types match {
    case Seq()                          => false
    case head::_ if check(head, token)  => true
    case _::tail                        => matchType(tail, token)
  }

  private def check(tokenType: TokenType, token: Token): Boolean =
    if (token.tokenType == EOF) false
    else token.tokenType == tokenType
}

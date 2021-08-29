package org.compiler.example
package interpreter.expr

import interpreter.InterResult.{InterResult, InterResultMonad, map}
import interpreter.Result
import interpreter.expr.InterExpr.evaluate
import interpreter.util.Converter.convertToDouble
import interpreter.util.Logical.isTruthy
import lexer.{BANG, MINUS, Token}
import parser.expr.Expr

object InterUnary {

  def interUnary(token: Token, rightExpr: Expr): InterResult[Result] =
    map(evaluate(rightExpr))(value => interUnaryValue(token, value))

  private def interUnaryValue(token: Token, rightValue: InterResult[Any]): InterResult[Any] = {

    implicit val line: Int = token.line

    token.tokenType match {
      case MINUS  => minusExpr(rightValue)
      case BANG   => rightValue.map(r => !isTruthy(r))
    }
  }

  def minusExpr(expr: InterResult[Any])(implicit line: Int): InterResult[Any] = for{
    r <- expr
    rd <- convertToDouble(r)
  } yield -rd
}

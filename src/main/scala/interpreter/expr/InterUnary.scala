package org.compiler.example
package interpreter.expr

import interpreter.Converter.convert
import interpreter.InterpreterResult.{InterResult, InterResultMonad}
import interpreter.expr.InterExpr.evaluate
import lexer.{BANG, MINUS, Token}
import parser.expr.Expr

object InterUnary {

  def interUnary(token: Token, rightExpr: Expr): InterResult[Any] = {

    implicit val line: Int = token.line
    val rightValue = evaluate(rightExpr)

    token.tokenType match {
      case MINUS  => minusExpr(rightValue)
      case BANG   => rightValue.map(r => !isTruthy(r))
    }
  }

  def minusExpr(expr: InterResult[Any])(implicit line: Int): InterResult[Any] = for{
    r <- expr
    rd <- convert[Double](r)
  } yield -rd

  private def isTruthy(value: Any): Boolean = value match {
    case Nil            => false
    case value: Boolean => value
    case _              => true
  }
}

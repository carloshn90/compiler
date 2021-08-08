package org.compiler.example
package interpreter.expr

import interpreter.InterpreterResult.{InterResult, InterResultMonad}
import interpreter.MathExpr.{isTruthy, minusExpr}
import interpreter.expr.InterExpr.evaluate
import lexer.{BANG, MINUS, Token}
import parser.expr.Expr

object InterUnary {

  def interUnary(token: Token, rightExpr: Expr): InterResult[Any] = {

    implicit val line: Int = token.line
    val rightValue = evaluate(rightExpr)

    token.tokenType match {
      case MINUS  => minusExpr(rightValue)
      case BANG   => rightValue.map(isTruthy)
    }
  }

}

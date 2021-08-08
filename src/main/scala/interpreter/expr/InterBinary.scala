package org.compiler.example
package interpreter.expr

import interpreter.InterpreterResult.{InterResult, InterResultMonad}
import interpreter.MathExpr.{addExpr, isEqual, logicExpr, mathExpr}
import interpreter.expr.InterExpr.evaluate
import lexer.{EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, MINUS, PLUS, SLASH, STAR, Token, BANG_EQUAL}
import parser.expr.Expr

object InterBinary {

  def interBinary(leftExpr: Expr, token: Token, rightExpr: Expr): InterResult[Any] = {

    val leftValue: InterResult[Any] = evaluate(leftExpr)
    val rightValue: InterResult[Any] = evaluate(rightExpr)
    implicit val line: Int = token.line

    token.tokenType match {
      case MINUS          => mathExpr(leftValue, rightValue)(_ - _)
      case SLASH          => mathExpr(leftValue, rightValue)(_ / _)
      case STAR           => mathExpr(leftValue, rightValue)(_ * _)
      case PLUS           => addExpr(leftValue, rightValue)
      case GREATER        => logicExpr(leftValue, rightValue)(_ > _)
      case GREATER_EQUAL  => logicExpr(leftValue, rightValue)(_ >= _)
      case LESS           => logicExpr(leftValue, rightValue)(_ < _)
      case LESS_EQUAL     => logicExpr(leftValue, rightValue)(_ <= _)
      case BANG_EQUAL     => leftValue.map2(rightValue)(!isEqual(_, _))
      case EQUAL_EQUAL    => leftValue.map2(rightValue)(isEqual)
    }
  }

}

package org.compiler.example
package interpreter.expr

import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.util.Converter.isTruthy
import lexer.{AND, OR, Token}
import parser.expr.Expr

object InterLogical {

  def interLogical(left: Expr, token: Token, right: Expr): InterResult[Any] =
    evaluate(left).flatMap(expr => logical(expr, token, right))

  private def logical(left: Any, token: Token, right: Expr): InterResult[Any] = {
    if (token.tokenType == OR && isTruthy(left)) unit(Right(left))
    else if(token.tokenType == AND && !isTruthy(left)) unit(Right(left))
    else evaluate(right)
  }
}

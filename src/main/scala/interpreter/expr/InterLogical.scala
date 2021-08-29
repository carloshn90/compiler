package org.compiler.example
package interpreter.expr

import interpreter.InterResult.{InterResult, InterResultMonad, flatMap, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.util.Logical.isTruthy
import interpreter.{InterpreterState, Result}
import lexer.{AND, OR, Token}
import parser.expr.Expr

object InterLogical {

  def interLogical(left: Expr, token: Token, right: Expr): InterResult[Result] =
    flatMap(evaluate(left))(l => interLogicalValue(l, token, right))

  private def interLogicalValue(left: InterResult[Any], token: Token, right: Expr): InterResult[Result] =
    left.flatMap(expr => logical(expr, token, right))

  private def logical(left: Any, token: Token, right: Expr): InterResult[Result] = {
    if (token.tokenType == OR && isTruthy(left)) unit(Right(InterpreterState(List(), Some(left))))
    else if(token.tokenType == AND && !isTruthy(left)) unit(Right(InterpreterState(List(), Some(left))))
    else evaluate(right)
  }
}

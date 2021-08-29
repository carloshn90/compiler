package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, map, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.{Environment, Result}
import lexer.Token
import parser.expr.Expr

object InterAssign {

  def interAssign(token: Token, expr: Expr): InterResult[Result] =
    map(evaluate(expr))(value => interAssignValue(token, value))

  private def interAssignValue(token: Token, exprValue: InterResult[Any]): InterResult[Any] = env => {
    val (right: Either[ErrorCompiler, Any], nextEnv: Environment) = exprValue(env)
    assignVar(token.lexeme, right)(token.line)(nextEnv)
  }

  private def assignVar(name: String, expr: Either[ErrorCompiler, Any])(implicit line: Int): InterResult[Any] = expr match {
    case Right(value) => assign(name, value)
    case _            => unit(expr)
  }

  private def assign(name: String, value: Any)(implicit line: Int): InterResult[Any] = env => env.assign(name, value) match {
    case Some(e) => (Right(value), e)
    case _       => (Left(ErrorCompiler(line, s"Undefined variable '$name'.")), env)
  }
}

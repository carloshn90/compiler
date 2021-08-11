package org.compiler.example
package interpreter.stmt

import interpreter.InterpreterResult.{InterResult, InterResultMonad}
import interpreter.expr.InterExpr.evaluate
import parser.expr.Expr

object InterExpression {

  def interExpression(expr: Expr): InterResult[Option[String]] = evaluate(expr).map(_ => None)
}

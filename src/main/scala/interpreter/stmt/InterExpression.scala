package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.InterResult
import interpreter.Result
import interpreter.expr.InterExpr.evaluate
import parser.expr.Expr

object InterExpression {

  def interExpression(expr: Expr): InterResult[Result] = evaluate(expr)
}

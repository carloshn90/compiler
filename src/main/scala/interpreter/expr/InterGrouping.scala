package org.compiler.example
package interpreter.expr

import interpreter.InterResult.InterResult
import interpreter.Result
import interpreter.expr.InterExpr.evaluate
import parser.expr.Expr

object InterGrouping {

  def interGrouping(expr: Expr): InterResult[Result] = evaluate(expr)
}

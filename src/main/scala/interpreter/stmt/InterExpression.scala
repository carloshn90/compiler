package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad}
import interpreter.expr.InterExpr.evaluate
import parser.expr.Expr

object InterExpression {

  def interExpression(expr: Expr): InterResult[List[String]] = evaluate(expr).map {
    case strList: List[String]  => strList
    case _                      => List()
  }
}

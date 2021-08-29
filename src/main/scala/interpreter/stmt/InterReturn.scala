package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.{Result, ReturnResult}
import parser.expr.Expr

object InterReturn {

  def interReturn(value: Expr): InterResult[Result] = value match {
    case Expr.None => unit(Right(ReturnResult(List(), None)))
    case _         => evaluate(value).map(v => ReturnResult(v.printList, v.value))
  }

}

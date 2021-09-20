package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad}
import interpreter.expr.InterExpr.evaluate
import interpreter.{InterpreterState, Result}
import parser.expr.Expr

object InterPrint {

  def interPrint(expr: Expr): InterResult[Result] = evaluate(expr)
    .map(e => e.value.map(stringify).map(str => InterpreterState(e.printList :+ str, None)).getOrElse(e))

  private def stringify(value: Any): String = value match {
    case Nil|null          => "Nil"
    case d: Double         => stringifyDouble(d)
    case v                 => v.toString
  }

  private def stringifyDouble(double: Double): String = {
    val doubleStr = double.toString
    if (doubleStr.endsWith(".0")) doubleStr.substring(0, doubleStr.length - 2)
    else doubleStr
  }
}

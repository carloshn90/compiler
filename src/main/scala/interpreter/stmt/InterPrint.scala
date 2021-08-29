package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad}
import interpreter.expr.InterExpr.evaluate
import parser.expr.Expr

object InterPrint {

  def interPrint(expr: Expr): InterResult[Option[String]] = evaluate(expr).map(e => e.value.map(stringify))

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

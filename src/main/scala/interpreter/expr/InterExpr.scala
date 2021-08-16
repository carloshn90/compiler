package org.compiler.example
package interpreter.expr

import interpreter.InterResult.InterResult
import interpreter.expr.InterAssign.interAssign
import interpreter.expr.InterBinary.interBinary
import interpreter.expr.InterGrouping.interGrouping
import interpreter.expr.InterLiteral.interLiteral
import interpreter.expr.InterLogical.interLogical
import interpreter.expr.InterUnary.interUnary
import interpreter.expr.InterVariable.interVariable
import parser.expr.{Assign, Binary, Expr, Grouping, Literal, Logical, Unary, Variable}

object InterExpr {

  def evaluate(expr: Expr): InterResult[Any] = expr match {
    case Binary(left, operator, right)  => interBinary(left, operator, right)
    case Grouping(expr)                 => interGrouping(expr)
    case Logical(left, token, right)    => interLogical(left, token, right)
    case Literal(value)                 => interLiteral(value)
    case Unary(token, right)            => interUnary(token, right)
    case Variable(token)                => interVariable(token)
    case Assign(token, expr)            => interAssign(token, expr)
  }
}

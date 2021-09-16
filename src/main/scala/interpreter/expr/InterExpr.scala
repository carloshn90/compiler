package org.compiler.example
package interpreter.expr

import interpreter.InterResult.InterResult
import interpreter.Result
import interpreter.expr.InterAssign.interAssign
import interpreter.expr.InterBinary.interBinary
import interpreter.expr.InterCall.interCall
import interpreter.expr.InterGet.interGet
import interpreter.expr.InterGrouping.interGrouping
import interpreter.expr.InterLiteral.interLiteral
import interpreter.expr.InterLogical.interLogical
import interpreter.expr.InterUnary.interUnary
import interpreter.expr.InterVariable.interVariable
import lexer.Token
import parser.expr.{Assign, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable, Get}

object InterExpr {

  def evaluate(expr: Expr): InterResult[Result] = expr match {
    case Binary(left, operator, right)              => interBinary(left, operator, right)
    case Call(funName, lastToken: Token, arguments) => interCall(funName, lastToken, arguments)
    case Get(expr, name)                            => interGet(expr, name)
    case Grouping(expr)                             => interGrouping(expr)
    case Logical(left, token, right)                => interLogical(left, token, right)
    case Literal(value)                             => interLiteral(value)
    case Unary(token, right)                        => interUnary(token, right)
    case Variable(token)                            => interVariable(token)
    case Assign(token, expr)                        => interAssign(token, expr)
  }
}

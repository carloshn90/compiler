package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.InterResult
import interpreter.stmt.InterExpression.interExpression
import interpreter.stmt.InterPrint.interPrint
import interpreter.stmt.InterVar.interVar
import parser.stmt.{Expression, Print, Stmt, Var}

object InterStmt {

  def execute(stmt: Stmt): InterResult[Option[String]] = stmt match {
    case Expression(expr)   => interExpression(expr)
    case Print(expr)        => interPrint(expr)
    case Var(token, expr)   => interVar(token, expr)
  }
}

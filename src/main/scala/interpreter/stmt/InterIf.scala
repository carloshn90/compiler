package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.stmt.InterStmt.execute
import interpreter.util.Logical.checkCondition
import interpreter.{InterpreterState, Result}
import parser.expr.Expr
import parser.stmt.Stmt

object InterIf {

  def interIf(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]): InterResult[Result] =
    evaluate(condition).flatMap(expr => ifCond(expr, thenBranch, elseBranch))

  def ifCond(condition: Result, thenBranch: Stmt, elseBranch: Option[Stmt]): InterResult[Result] = {
    if (checkCondition(condition)) execute(thenBranch)
    else if (elseBranch.isDefined) execute(elseBranch.get)
    else unit(Right(InterpreterState(List(), None)))
  }
}

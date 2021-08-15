package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.stmt.InterStmt.execute
import interpreter.util.Converter.isTruthy
import parser.expr.Expr
import parser.stmt.Stmt

object InterIf {

  def interIf(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]): InterResult[List[String]] =
    evaluate(condition).flatMap(expr => ifCond(expr, thenBranch, elseBranch))

  def ifCond(condition: Any, thenBranch: Stmt, elseBranch: Option[Stmt]): InterResult[List[String]] = {
    if (isTruthy(condition)) execute(thenBranch)
    else if (elseBranch.isDefined) execute(elseBranch.get)
    else unit(Right(List()))
  }
}

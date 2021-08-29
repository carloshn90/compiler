package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.stmt.InterStmt.execute
import interpreter.util.Logical.checkCondition
import interpreter.{InterpreterState, Result}
import parser.expr.Expr
import parser.stmt.Stmt

object InterWhile {

  def interWhile(condition: Expr,  body: Stmt): InterResult[Result] = env =>
    evaluate(condition).flatMap { (cond: Result) =>
      if(checkCondition(cond)) executeWhileBody(condition, body)
      else unit(Right(InterpreterState(List(), None)))
    }(env)

  private def executeWhileBody(condition: Expr, body: Stmt): InterResult[Result] = execute(body)
    .flatMap { bodyResult =>
      interWhile(condition, body).map(carry => InterpreterState(bodyResult.printList ::: carry.printList, bodyResult.value))
    }
}

package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.stmt.InterStmt.execute
import interpreter.util.Converter.isTruthy
import parser.expr.Expr
import parser.stmt.Stmt

object InterWhile {

  def interWhile(condition: Expr,  body: Stmt): InterResult[List[String]] = env =>
    evaluate(condition).flatMap { cond =>
      if(isTruthy(cond)) executeWhileBody(condition, body)
      else unit(Right(List()))
    }(env)

  private def executeWhileBody(condition: Expr, body: Stmt): InterResult[List[String]] = execute(body)
    .flatMap { bodyResult =>
      interWhile(condition, body).map(carry => carry ::: bodyResult)
    }
}

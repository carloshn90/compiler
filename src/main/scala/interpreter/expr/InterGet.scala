package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.`class`.InterInstance
import interpreter.expr.InterExpr.evaluate
import interpreter.{InterpreterState, Result}
import lexer.Token
import parser.expr.Expr

object InterGet {

  def interGet(expr: Expr, name: Token): InterResult[Result] = {
    evaluate(expr).flatMap(result => get(result, name))
  }

  private def get(result: Result, name: Token): InterResult[Result] = result match {
    case InterpreterState(prints, Some(inst: InterInstance)) => unit(inst.get(name).map(v => InterpreterState(prints, Some(v))))
    case _                                                   => unit(Left(ErrorCompiler(name.line, "Only instances have methods.")))
  }

}

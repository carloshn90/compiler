package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.InterResult.InterResult
import interpreter.expr.InterExpr.evaluate
import interpreter.{Environment, InterpreterState, Result}
import lexer.Token
import parser.expr.Expr

object InterVar {

  def interVar(token: Token, expr: Expr): InterResult[Option[String]] = env => {
    val (right: Either[ErrorCompiler, Result], nextEnv: Environment) = evaluate(expr)(env)
    right match {
      case Right(InterpreterState(_, None))         => defineVar(token.lexeme, Nil)(nextEnv)
      case Right(InterpreterState(_, Some(value)))  => defineVar(token.lexeme, value)(nextEnv)
      case Left(err)                      => (Left(err), nextEnv)
    }
  }

  private def defineVar(name: String, value: Any): InterResult[Option[String]] = env => (Right(None), env.define(name, value))
}

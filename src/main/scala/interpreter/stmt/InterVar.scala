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
      case Right(InterpreterState(_, None))         => defineVar(token, Nil)(nextEnv)
      case Right(InterpreterState(_, Some(value)))  => defineVar(token, value)(nextEnv)
      case Left(err)                                => (Left(err), nextEnv)
    }
  }

  private def defineVar(name: Token, value: Any): InterResult[Option[String]] = env => env.define(name, value) match {
    case Right(e)   => (Right(None), e)
    case Left(err)  => (Left(err), env)
  }
}

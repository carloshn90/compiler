package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.InterResult.InterResult
import interpreter.expr.InterExpr.evaluate
import lexer.Token
import parser.expr.Expr

object InterVar {

  def interVar(token: Token, expr: Expr): InterResult[Option[String]] = env => {
    val (right: Either[ErrorCompiler, Any], nextEnv: Environment) = evaluate(expr)(env)
    right match {
      case Right(null)   => defineVar(token.lexeme, Nil)(nextEnv)
      case Right(value)  => defineVar(token.lexeme, value)(nextEnv)
      case Left(err)     => (Left(err), nextEnv)
    }
  }

  private def defineVar(name: String, value: Any): InterResult[Option[String]] = env => (Right(None), env.define(name, value))
}
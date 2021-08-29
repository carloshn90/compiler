package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.{Environment, InterpreterState, Result}
import lexer.Token

import cats.implicits.toBifunctorOps

object InterVariable {

  def interVariable(token: Token): InterResult[Result] = env => unit(getValue(env, token))
    .map(t => InterpreterState(List(), Some(t)))(env)

  private def getValue(env: Environment, token: Token): Either[ErrorCompiler, Any] =
    env.get(token.lexeme)
      .leftMap(err => ErrorCompiler(token.line, err))
}

package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.Environment
import interpreter.InterpreterResult.{InterResult, unit}
import lexer.Token

import cats.implicits.toBifunctorOps

object InterVariable {

  def interVariable(token: Token): InterResult[Any] = env => unit(getValue(env, token))(env)

  private def getValue(env: Environment, token: Token): Either[ErrorCompiler, Any] =
    env.get(token)
      .leftMap(err => ErrorCompiler(token.line, err))
}

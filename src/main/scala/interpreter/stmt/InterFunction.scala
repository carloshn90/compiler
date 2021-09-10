package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.InterResult
import interpreter.function.InterFunction
import interpreter.{InterpreterState, Result}
import lexer.Token
import parser.stmt.{Function, Stmt}

object InterFunction {

  def interFunction(name: Token, params: List[Token], body: List[Stmt]): InterResult[Result] = env => {
    val interFunction: InterFunction = new InterFunction(Function(name, params, body), env)
    env.define(name, interFunction) match {
      case Right(environment) => (Right(InterpreterState(List(), None)), environment)
      case Left(err)          => (Left(err), env)
    }
  }
}

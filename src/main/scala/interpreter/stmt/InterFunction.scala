package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, define}
import interpreter.Result
import interpreter.function.InterFunction
import lexer.Token
import parser.stmt.{Function, Stmt}

object InterFunction {

  def interFunction(name: Token, params: List[Token], body: List[Stmt]): InterResult[Result] = env => {
    val interFunction: InterFunction = new InterFunction(Function(name, params, body), env)
    define(name, interFunction)(env)
  }
}

package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.InterResult
import interpreter.function.InterFunction
import lexer.Token
import parser.stmt.{Function, Stmt}

object InterFunction {

  def interFunction(name: Token, params: List[Token], body: List[Stmt]): InterResult[List[String]] = env => {
    val interFunction: InterFunction = new InterFunction(Function(name, params, body))
    (Right(List()), env.define(name.lexeme, interFunction))
  }
}

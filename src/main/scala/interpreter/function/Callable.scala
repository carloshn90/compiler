package org.compiler.example
package interpreter.function

import interpreter.InterResult.InterResult
import interpreter.Result
import lexer.Token

trait Callable {
  def argumentSize: Int

  def call(funName: Token, arguments: List[Result]): InterResult[Result]
}

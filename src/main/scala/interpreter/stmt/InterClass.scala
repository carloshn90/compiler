package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.InterResult
import interpreter.{Result, ReturnResult}
import interpreter.`class`.InterClass
import lexer.Token
import parser.stmt.Function

object InterClass {

  def interClass(name: Token, methods: List[Function]): InterResult[Result] = {
    val interClass: InterClass = new InterClass(name.lexeme)
    defineClass(name, interClass)
  }

  private def defineClass(name: Token, interClass: InterClass): InterResult[Result] = env =>
    env.define(name, interClass) match {
      case Right(e)   => (Right(ReturnResult(List(), None)), e)
      case Left(err)  => (Left(err), env)
    }
}

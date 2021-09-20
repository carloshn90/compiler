package org.compiler.example
package interpreter.`class`

import error.ErrorCompiler
import interpreter.function.InterFunction
import lexer.Token

class InterInstance(methods: Map[String, InterFunction]) {

  def get(token: Token): Either[ErrorCompiler, InterFunction] = methods.get(token.lexeme) match {
    case Some(value)  => Right(value)
    case None         => Left(ErrorCompiler(token.line, s"Undefined property '${token.lexeme}'."))
  }
}

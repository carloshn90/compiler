package org.compiler.example
package interpreter.`class`

import error.ErrorCompiler
import lexer.Token

class InterInstance(methods: Map[String, Any]) {

  def get(token: Token): Either[ErrorCompiler, Any] = methods.get(token.lexeme) match {
    case Some(value)  => Right(value)
    case None         => Left(ErrorCompiler(token.line, s"Undefined property '${token.lexeme}'."))
  }
}

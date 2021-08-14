package org.compiler.example
package interpreter

import lexer.Token

class Environment(values: Map[String, Any] = Map()) {

  def size: Int = values.size

  def define(name: String, value: Any): Environment =
    new Environment(values + (name -> value))

  def assign(name: String, value: Any): Option[Environment] = values
      .get(name)
      .map(_ => define(name, value))

  def get(token: Token): Either[String, Any] =
    values.get(token.lexeme)
      .toRight(s"Undefined variable '${token.lexeme}'.")
}

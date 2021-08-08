package org.compiler.example
package lexer

import scala.annotation.tailrec

case class Token(tokenType: TokenType, lexeme: String, line: Int, literal: Option[Any] = None)

object Token {

  @tailrec
  def matchType(types: Seq[TokenType], token: Token): Boolean = types match {
    case Seq()                          => false
    case head::_ if check(head, token)  => true
    case _::tail                        => matchType(tail, token)
  }

  private def check(tokenType: TokenType, token: Token): Boolean =
    if (token.tokenType == EOF) false
    else token.tokenType == tokenType
}

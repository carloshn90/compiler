package org.compiler.example
package lexer

case class Token(tokenType: TokenType, lexeme: String, line: Int, literal: Option[Object] = None)

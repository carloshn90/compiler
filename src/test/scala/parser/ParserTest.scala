package org.compiler.example
package parser

import error.ErrorCompiler
import lexer.{EOF, IF, LEFT_PAREN, PRINT, RIGHT_PAREN, STRING, TRUE, Token}
import parser.stmt.Stmt

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSuite with Matchers {

  test("Parsing wrong if branch expression, should return error") {

    val tokenList: List[Token] = List(
      Token(IF, "if", 1, None),
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello", 2, Some("Hello")),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val parserResult: Either[ErrorCompiler, List[Stmt]] = parser.parser()(tokenList)

    parserResult shouldBe Left(ErrorCompiler(2, "Expect ';' after value."))
  }

}

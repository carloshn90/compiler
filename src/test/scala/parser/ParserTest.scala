package org.compiler.example
package parser

import error.ErrorCompiler
import lexer.{EOF, IF, LEFT_PAREN, NUMBER, PRINT, RETURN, RIGHT_PAREN, STRING, TRUE, Token}
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

  test("Parsing wrong return statement, should return error expression not allowed here") {

    val tokenList: List[Token] = List(
      Token(RETURN, "return", 1, None),
      Token(NUMBER, "2.33", 1, Some(2.33)),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val parserResult: Either[ErrorCompiler, List[Stmt]] = parser.parser()(tokenList)

    parserResult shouldBe Left(ErrorCompiler(1, "the expression 'return' is not allowed here."))
  }

}

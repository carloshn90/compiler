package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer._
import parser.expr.Literal
import parser.expr.ParserExpr.ParserExpr
import parser.stmt.ParserPrint.parserPrint
import parser.stmt.ParserStmt.StmtResult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserPrintTest extends AnyFunSuite with Matchers {

  test("Parsing print expression, should return print statement") {
    val expectedToken: Token = Token(STRING, "Hello", 1, Some("Hello"))
    val tokenList: List[Token] = List(
      expectedToken,
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val (exprResult: StmtResult, tokenListResult: List[Token]) =
      parserPrint(expression(expectedToken))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Print(Literal(expectedToken)))
  }

  test("Parsing print expression missing SEMICOLON, should return an error") {
    val expectedToken: Token = Token(STRING, "Hello", 1, Some("Hello"))
    val tokenList: List[Token] = List(
      expectedToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: StmtResult, tokenListResult: List[Token]) =
      parserPrint(expression(expectedToken))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Left(ErrorCompiler(1, "Expect ';' after value."))
  }

  private def expression(expected: Token): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(Literal(expected)), tokenList.tail)
  }

}

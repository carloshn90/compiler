package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer._
import parser.expr.{Expr, Literal}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.ParserGrammar
import parser.stmt.ParserPrint.parserPrint

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

    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) =
      parserPrint(expression(expectedToken))(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Print(Literal(expectedToken)))
  }

  test("Parsing print expression missing SEMICOLON, should return an error") {
    val expectedToken: Token = Token(STRING, "Hello", 1, Some("Hello"))
    val tokenList: List[Token] = List(
      expectedToken,
      Token(EOF, "", 1, None)
    )

    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) =
      parserPrint(expression(expectedToken))(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect ';' after value."))
  }

  private def expression(expected: Token): ParserGrammar[Expr] = tokenList => {
    tokenList.head shouldBe expected
    (Right(Literal(expected)), tokenList.tail)
  }

}

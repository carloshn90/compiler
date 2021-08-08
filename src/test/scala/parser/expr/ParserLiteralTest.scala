package org.compiler.example
package parser.expr

import lexer.{EOF, NUMBER, STRING, Token}
import parser.expr.ParserLiteral.parserLiteral
import parser.grammar.GrammarResult.GrammarResult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserLiteralTest extends AnyFunSuite with Matchers {

  test("Parsing literal expression NUMBER, should return literal with a NUMBER") {
    val tokenList: List[Token] = List(
      Token(NUMBER, "2.33", 2, Some(2.33)),
      Token(EOF, "", 1, None)
    )

    val  (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLiteral(2.33)(tokenList)
    tokenListResult should have size 1
    grammarResult shouldBe Right(Literal(2.33))
  }

  test("Parsing literal expression STRING, should return literal with a String") {
    val tokenList: List[Token] = List(
      Token(STRING, "hello", 1, Some("hello")),
      Token(EOF, "", 2, None)
    )

    val  (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLiteral("hello")(tokenList)
    tokenListResult should have size 1
    grammarResult shouldBe Right(Literal("hello"))
  }
}

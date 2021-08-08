package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer._
import parser.expr.ParserGrouping.parserGrouping
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.ParserGrammar

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserGroupingTest extends AnyFunSuite with Matchers {

  test("Parsing grouping expression with missing close parenthesis, should return error") {
    val expectedToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val expectedExprResult: Expr = Literal(2.33)
    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      expectedToken,
      Token(EOF, "", 1, None)
    )

    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserGrouping()(() => checkToken(expectedToken, expectedExprResult))(tokenList)
    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, s"Expect ')' after expression ''"))
  }

  test("Parsing grouping expression between parenthesis, should return grouping expression") {
    val expectedToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val literalResult: Expr = Literal(2.33)
    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      expectedToken,
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserGrouping()(() => checkToken(expectedToken, literalResult))(tokenList)
    tokenListResult should have size 1
    grammarResult shouldBe Right(Grouping(literalResult))
  }

  private def checkToken(expected: Token, result: Expr): ParserGrammar[Expr] = tokenList => {
    tokenList.head shouldBe expected
    (Right(result), tokenList.tail)
  }

}

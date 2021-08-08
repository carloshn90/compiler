package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{EOF, LEFT_PAREN, NUMBER, RIGHT_PAREN, SEMICOLON, Token}
import parser.expr.ParserExpr.{ExprResult, ParserExpr}
import parser.expr.ParserGrouping.parserGrouping

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

    val (exprResult: ExprResult, tokenListResult: List[Token]) = parserGrouping()(() => checkToken(expectedToken, expectedExprResult))(tokenList)
    tokenListResult should have size 1
    exprResult shouldBe Left(ErrorCompiler(1, s"Expect ')' after expression ''"))
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

    val (exprResult: ExprResult, tokenListResult: List[Token]) = parserGrouping()(() => checkToken(expectedToken, literalResult))(tokenList)
    tokenListResult should have size 1
    exprResult shouldBe Right(Grouping(literalResult))
  }

  private def checkToken(expected: Token, result: Expr): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(result), tokenList.tail)
  }

}

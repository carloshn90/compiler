package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{Token, _}
import parser.expr.ParserExpr.{ExprResult, ParserExpr}
import parser.expr.ParserUnary.parserUnary

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserUnaryTest extends AnyFunSuite with Matchers {

  test("Parsing unary MINUS expression, should return unary MINUS expression") {
    val expectedToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val expectedExprResult: Expr = Literal(2.33)
    val types: Seq[TokenType] = Seq(MINUS)
    val tokenList: List[Token] = List(
      Token(MINUS, "-", 1, None),
      expectedToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserUnary(types)(() => checkToken(expectedToken, expectedExprResult), errorParserExpr)(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Unary(Token(MINUS, "-", 1, None), expectedExprResult))
  }

  test("Parsing no unary expression, should return no unary expression") {
    val expectedToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val expectedExprResult: Expr = Literal(2.33)
    val types: Seq[TokenType] = Seq(BANG, MINUS)
    val tokenList: List[Token] = List(
      expectedToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserUnary(types)(errorParserExpr, () => checkToken(expectedToken, expectedExprResult))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(expectedExprResult)
  }

  private def checkToken(expected: Token, result: Expr): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(result), tokenList.tail)
  }

  private def errorParserExpr(): ParserExpr = tokenList => (Left(ErrorCompiler(0, "Error")), tokenList.tail)
}

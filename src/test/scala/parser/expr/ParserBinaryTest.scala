package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer._
import parser.expr.ParserBinary.parserBinary
import parser.expr.ParserExpr.{ExprResult, ParserExpr}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserBinaryTest extends AnyFunSuite with Matchers {

  test("Parsing binary expression, should return binary with left expression, token and right expression") {
    val expectedToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val expectedExprResult: Expr = Literal(2.33)
    val types: Seq[TokenType] = Seq(PLUS)
    val tokenList: List[Token] = List(
      Token(PLUS, "+", 1, None),
      expectedToken,
      Token(EOF, "", 1, None)
    )
    val leftExprResult: ParserExpr = tokenList => (Right(expectedExprResult), tokenList)

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserBinary(leftExprResult,types)(() => checkToken(expectedToken, expectedExprResult))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Binary(expectedExprResult, Token(PLUS, "+", 1, None), expectedExprResult))
  }

  test("Parsing No binary expression, should return literal expression") {
    val expectedExprResult: Expr = Literal(2.33)
    val types: Seq[TokenType] = Seq(PLUS)
    val tokenList: List[Token] = List(
      Token(EOF, "", 1, None)
    )
    val leftExprResult: ParserExpr = tokenList => (Right(expectedExprResult), tokenList)

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserBinary(leftExprResult, types)(errorParserExpr)(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(expectedExprResult)
  }

  test("Left expression contains an error, should return left error") {
    val leftExprWithError: ErrorCompiler = ErrorCompiler(0, "Left error")
    val types: Seq[TokenType] = Seq(PLUS)
    val tokenList: List[Token] = List(
      Token(PLUS, "+", 1, None),
      Token(EOF, "", 1, None)
    )
    val leftExprResultWithError: ParserExpr = tokenList => (Left(leftExprWithError), tokenList)

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserBinary(leftExprResultWithError, types)(errorParserExpr)(tokenList)

    tokenListResult should have size 2
    exprResult shouldBe Left(leftExprWithError)
  }

  private def checkToken(expected: Token, result: Expr): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(result), tokenList.tail)
  }

  private def errorParserExpr(): ParserExpr = tokenList => (Left(ErrorCompiler(0, "Error")), tokenList.tail)
}

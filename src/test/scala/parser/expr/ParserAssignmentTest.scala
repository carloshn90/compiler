package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer._
import parser.expr.ParserAssignment.parserAssignment
import parser.expr.ParserExpr.{ExprResult, ParserExpr}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserAssignmentTest extends AnyFunSuite with Matchers {

  test("Parsing assigment with a number variable, should return a variable with the value") {

    val identifierToken: Token = Token(IDENTIFIER, "variableName", 0, None)
    val numberToken: Token = Token(NUMBER, "1.14", 1, Some(1.14))
    val tokenList: List[Token] = List(
      identifierToken,
      Token(EQUAL, "=", 1, None),
      numberToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserAssignment(equality(identifierToken), Seq(EQUAL))(() => assignment(numberToken))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Assign(identifierToken, Literal(1.14)))
  }

  test("Parsing assigment with erroneous expression, should return a error") {

    val identifierToken: Token = Token(IDENTIFIER, "variableName", 0, None)
    val numberToken: Token = Token(NUMBER, "1.14", 1, Some(1.14))
    val tokenList: List[Token] = List(
      identifierToken,
      Token(EQUAL, "=", 1, None),
      numberToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserAssignment(erroneousExpression(identifierToken), Seq(EQUAL))(() => assignment(numberToken))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Left(ErrorCompiler(1, "Invalid assignment target."))
  }

  test("Parsing only left expression, should return left expression") {

    val numberToken: Token = Token(NUMBER, "1.14", 1, Some(1.14))
    val tokenList: List[Token] = List(
      numberToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: ExprResult, tokenListResult: List[Token]) =
      parserAssignment(equality(numberToken), Seq(EQUAL))(() => errorIfCalled())(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Variable(numberToken))
  }

  private def equality(expected: Token): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(Variable(expected)), tokenList.tail)
  }

  private def erroneousExpression(expected: Token): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(Literal(expected)), tokenList.tail)
  }

  private def assignment(expected: Token): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(Literal(1.14)), tokenList.tail)
  }

  private def errorIfCalled(): ParserExpr = tokenList => {
    fail("Assign shouldn't parser right expression")
    (Right(Literal()), tokenList)
  }
}

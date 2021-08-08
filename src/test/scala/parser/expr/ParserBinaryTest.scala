package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer._
import parser.expr.ParserBinary.parserBinary
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.ParserGrammar

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
    val leftExprResult: ParserGrammar[Expr] = tokenList => (Right(expectedExprResult), tokenList)

    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) =
      parserBinary(leftExprResult,types)(() => checkToken(expectedToken, expectedExprResult))(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Binary(expectedExprResult, Token(PLUS, "+", 1, None), expectedExprResult))
  }

  test("Parsing No binary expression, should return literal expression") {
    val expectedExprResult: Expr = Literal(2.33)
    val types: Seq[TokenType] = Seq(PLUS)
    val tokenList: List[Token] = List(
      Token(EOF, "", 1, None)
    )
    val leftExprResult: ParserGrammar[Expr] = tokenList => (Right(expectedExprResult), tokenList)

    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) =
      parserBinary(leftExprResult, types)(errorParserExpr)(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(expectedExprResult)
  }

  test("Left expression contains an error, should return left error") {
    val leftExprWithError: ErrorCompiler = ErrorCompiler(0, "Left error")
    val types: Seq[TokenType] = Seq(PLUS)
    val tokenList: List[Token] = List(
      Token(PLUS, "+", 1, None),
      Token(EOF, "", 1, None)
    )
    val leftExprResultWithError: ParserGrammar[Expr] = tokenList => (Left(leftExprWithError), tokenList)

    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) =
      parserBinary(leftExprResultWithError, types)(errorParserExpr)(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(leftExprWithError)
  }

  private def checkToken(expected: Token, result: Expr): ParserGrammar[Expr] = tokenList => {
    tokenList.head shouldBe expected
    (Right(result), tokenList.tail)
  }

  private def errorParserExpr(): ParserGrammar[Expr] = tokenList => (Left(ErrorCompiler(0, "Error")), tokenList.tail)
}

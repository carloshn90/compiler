package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer._
import parser.expr.Literal
import parser.expr.ParserExpr.ParserExpr
import parser.stmt.ParserStmt.StmtResult
import parser.stmt.ParserVar.parserVar

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserVarTest extends AnyFunSuite with Matchers {

  test("Parsing a var statement with value, should return a var with the value initializer") {

    val identifierToken: Token = Token(IDENTIFIER, "variableName", 0, Some("value"))
    val valueToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val tokenList: List[Token] = List(
      identifierToken,
      Token(EQUAL, "=", 1, None),
      valueToken,
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val (exprResult: StmtResult, tokenListResult: List[Token]) = parserVar(expression(valueToken))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Var(identifierToken, Literal(valueToken)))
  }

  test("Parsing a var statement without value, should return a var with the null value") {

    val identifierToken: Token = Token(IDENTIFIER, "variableName", 0, Some("value"))
    val tokenList: List[Token] = List(
      identifierToken,
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val (exprResult: StmtResult, tokenListResult: List[Token]) = parserVar(errorParserExpr())(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Right(Var(identifierToken, null))
  }

  test("Parsing a var statement with wrong expression, should return error") {

    val valueToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val tokenList: List[Token] = List(
      valueToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: StmtResult, tokenListResult: List[Token]) = parserVar(expression(valueToken))(tokenList)

    tokenListResult should have size 2
    exprResult shouldBe Left(ErrorCompiler(1, "Expect variable name."))
  }

  test("Parsing a var statement missing semicolon, should return an error") {

    val identifierToken: Token = Token(IDENTIFIER, "variableName", 0, Some("value"))
    val valueToken: Token = Token(NUMBER, "2.33", 1, Some(2.33))
    val tokenList: List[Token] = List(
      identifierToken,
      Token(EQUAL, "=", 1, None),
      valueToken,
      Token(EOF, "", 1, None)
    )

    val (exprResult: StmtResult, tokenListResult: List[Token]) = parserVar(expression(valueToken))(tokenList)

    tokenListResult should have size 1
    exprResult shouldBe Left(ErrorCompiler(1, "Expect ';' after value."))
  }

  private def expression(expected: Token): ParserExpr = tokenList => {
    tokenList.head shouldBe expected
    (Right(Literal(expected)), tokenList.tail)
  }

  private def errorParserExpr(): ParserExpr = tokenList => (Left(ErrorCompiler(0, "Error")), tokenList.tail)
}

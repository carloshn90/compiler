package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer._
import parser.expr.{Expr, Literal}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.ParserGrammar
import parser.stmt.ParserExpression.parserExpression

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserExpressionTest extends AnyFunSuite with Matchers {

  test("Parsing statement expression, return expression with a number literal") {
    val expectedToken: Token = Token(NUMBER, "1.578", 1, Some(1.578))
    val tokenList: List[Token] = List(
      expectedToken,
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) =
      parserExpression(expression(expectedToken))(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Expression(Literal(expectedToken)))
  }

  test("Parsing statement expression missing semicolon, return semicolon error") {
    val expectedToken: Token = Token(NUMBER, "1.578", 1, Some(1.578))
    val tokenList: List[Token] = List(
      expectedToken,
      Token(EOF, "", 2, None)
    )

    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) =
      parserExpression(expression(expectedToken))(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect ';' after value."))
  }

  private def expression(expected: Token): ParserGrammar[Expr] = tokenList => {
    tokenList.head shouldBe expected
    (Right(Literal(expected)), tokenList.tail)
  }

}

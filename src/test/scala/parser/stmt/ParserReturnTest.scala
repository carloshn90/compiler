package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, NUMBER, PRINT, RETURN, SEMICOLON, Token}
import parser.Parser
import parser.expr.{Expr, Literal}
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserReturn.parserReturn

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserReturnTest extends AnyFunSuite with Matchers {

  test("Parsing return expression, should return a value") {
    val tokenList: List[Token] = List(
      Token(RETURN, "return", 1, None),
      Token(NUMBER, "8.1", 1, Some(8.1)),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserReturn(parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Return(Token(RETURN, "return", 1, None), Literal(8.1)))
  }

  test("Parsing return nothing, should return a None expression") {
    val tokenList: List[Token] = List(
      Token(RETURN, "return", 1, None),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserReturn(parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Return(Token(RETURN, "return", 1, None), Expr.None))
  }

  test("Error parsing expression, should return an error") {
    val tokenList: List[Token] = List(
      Token(RETURN, "return", 1, None),
      Token(PRINT, "print", 1, None),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserReturn(parser.expression())(tokenList)

    tokenListResult should have size 3
    grammarResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }

  test("Error parsing nothing missing semicolon, should return an error") {
    val tokenList: List[Token] = List(
      Token(RETURN, "return", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserReturn(parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect ';' after return value."))
  }

  test("Error parsing expression missing semicolon, should return an error") {
    val tokenList: List[Token] = List(
      Token(RETURN, "return", 1, None),
      Token(NUMBER, "8.1", 1, Some(8.1)),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserReturn(parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect ';' after return value."))
  }
}

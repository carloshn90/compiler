package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, LEFT_PAREN, NUMBER, PRINT, RIGHT_BRACE, SEMICOLON, STRING, Token}
import parser.Parser
import parser.expr.Literal
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserBlock.parserBlock

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserBlockTest extends AnyFunSuite with Matchers {

  test("Parsing block expression, should return Block with the list of statement") {

    val tokenList: List[Token] = List(
      Token(PRINT, "print", 1, None),
      Token(STRING, "Hello", 1, Some("Hello")),
      Token(SEMICOLON, ";", 1, None),
      Token(PRINT, "print", 2, None),
      Token(NUMBER, "2.33", 2, Some(2.33)),
      Token(SEMICOLON, ";", 1, None),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(EOF, "", 4, None)
    )

    val expectedResult: Block = Block(List(
      Print(Literal("Hello")),
      Print(Literal(2.33)),
    ))

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserBlock(parser.declaration())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(expectedResult)
  }

  test("Error parsing block expression without right brace, should return error") {

    val tokenList: List[Token] = List(
      Token(PRINT, "print", 1, None),
      Token(STRING, "Hello", 1, Some("Hello")),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val grammarResult: GrammarResult[Stmt] = parserBlock(parser.declaration())(tokenList)._1

    grammarResult shouldBe Left(ErrorCompiler(1, "Expect '}' after block."))
  }

  test("Error Parsing statement, should return error") {

    val tokenList: List[Token] = List(
      Token(PRINT, "print", 1, None),
      Token(STRING, "Hello", 1, Some("Hello")),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val grammarResult: GrammarResult[Stmt] = parserBlock(parser.declaration())(tokenList)._1

    grammarResult shouldBe Left(ErrorCompiler(1, "Expect ';' after value."))
  }

  test("Error Parsing expression, should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "Hello", 1, Some("Hello")),
      Token(EOF, "", 4, None)
    )

    val parser: Parser = new Parser()
    val grammarResult: GrammarResult[Stmt] = parserBlock(parser.declaration())(tokenList)._1

    grammarResult shouldBe Left(ErrorCompiler(4, "Expect ')' after expression ''"))
  }

}

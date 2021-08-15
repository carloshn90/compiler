package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{ELSE, EOF, LEFT_BRACE, LEFT_PAREN, PRINT, RIGHT_BRACE, RIGHT_PAREN, SEMICOLON, STRING, TRUE, Token}
import parser.Parser
import parser.expr.Literal
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserIf.parserIf

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserIfTest extends AnyFunSuite with Matchers {

  test("Parsing wrong if condition expression, should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(PRINT, "print", 1, None),
      Token(STRING, "Hello", 1, Some("Hello")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 4
    parserResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }

  test("Parsing wrong if branch expression, should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello", 2, Some("Hello")),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Left(ErrorCompiler(2, "Expect ';' after value."))
  }

  test("Parsing if statement missing left parenthesis, should return error") {

    val tokenList: List[Token] = List(
      Token(PRINT, "print", 1, None),
    )

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Left(ErrorCompiler(1, "Expect '(' after 'if'."))
  }

  test("Parsing if statement missing right parenthesis, should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(PRINT, "print", 1, None),
    )

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Left(ErrorCompiler(1, "Expect ')' after if condition."))
  }

  test("Parsing if statement with code block, should return if statement without else block") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(LEFT_BRACE, "{", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello", 2, Some("Hello")),
      Token(SEMICOLON, ";", 2, None),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(EOF, "", 4, None)
    )

    val expected: Stmt = If(Literal(true), Block(List(Print(Literal("Hello")))), None)

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Right(expected)
  }

  test("Parsing simple if statement, should return if statement without else block") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello", 2, Some("Hello")),
      Token(SEMICOLON, ";", 2, None),
      Token(EOF, "", 3, None)
    )

    val expected: Stmt = If(Literal(true), Print(Literal("Hello")), None)

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Right(expected)
  }

  test("Parsing wrong else statement, should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello if", 2, Some("Hello if")),
      Token(SEMICOLON, ";", 2, None),
      Token(ELSE, "else", 3, None),
      Token(PRINT, "print", 4, None),
      Token(STRING, "Hello else", 4, Some("Hello else")),
      Token(EOF, "", 5, None)
    )

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Left(ErrorCompiler(4, "Expect ';' after value."))
  }

  test("Parsing simple if-else statement, should return simple if-else statement") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello if", 2, Some("Hello if")),
      Token(SEMICOLON, ";", 2, None),
      Token(ELSE, "else", 3, None),
      Token(PRINT, "print", 4, None),
      Token(STRING, "Hello else", 4, Some("Hello else")),
      Token(SEMICOLON, ";", 4, None),
      Token(EOF, "", 5, None)
    )

    val expected: Stmt = If(Literal(true), Print(Literal("Hello if")), Some(Print(Literal("Hello else"))))

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Right(expected)
  }

  test("Parsing if-else statement with block, should return simple if-else statement with block") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1,None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(LEFT_BRACE, "{", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "Hello if", 2, Some("Hello if")),
      Token(SEMICOLON, ";", 2, None),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(ELSE, "else", 4, None),
      Token(LEFT_BRACE, "{", 4, None),
      Token(PRINT, "print", 5, None),
      Token(STRING, "Hello else", 5, Some("Hello else")),
      Token(SEMICOLON, ";", 5, None),
      Token(RIGHT_BRACE, "}", 6, None),
      Token(EOF, "", 7, None)
    )

    val expected: Stmt = If(Literal(true), Block(List(Print(Literal("Hello if")))), Some(Block(List(Print(Literal("Hello else"))))))

    val parser: Parser = new Parser()
    val (parserResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserIf(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    parserResult shouldBe Right(expected)
  }
}

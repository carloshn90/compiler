package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, LEFT_PAREN, PRINT, RIGHT_PAREN, SEMICOLON, STRING, TRUE, Token}
import parser.Parser
import parser.expr.Literal
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserWhileStmt.parserWhileStmt

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserWhileStmtTest extends AnyFunSuite with Matchers {

  test("Parsing a while statement, should return a while statement") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1, Some("true")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "hello", 2, Some("hello")),
      Token(SEMICOLON, ";", 2, None),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserWhileStmt(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(While(Literal(true), Print(Literal("hello"))))
  }

  test("Error left parenthesis missing, should return an error") {

    val tokenList: List[Token] = List(
      Token(TRUE, "true", 1, Some("true")),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserWhileStmt(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect '(' after 'while'."))
  }

  test("Error right parenthesis missing, should return an error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1, Some("true")),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserWhileStmt(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(3, "Expect ')' after 'condition'."))
  }

  test("Error parsing condition, should return an error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(PRINT, "print", 1, None),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserWhileStmt(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }

  test("Error parsing body, should return an error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(TRUE, "true", 1, Some("true")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 2, None),
      Token(STRING, "hello", 2, Some("hello")),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserWhileStmt(parser.expression(), parser.statement())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect ';' after value."))
  }

}

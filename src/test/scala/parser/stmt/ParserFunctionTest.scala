package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{COMMA, EOF, IDENTIFIER, LEFT_BRACE, LEFT_PAREN, NUMBER, PRINT, RIGHT_BRACE, RIGHT_PAREN, SEMICOLON, Token}
import parser.Parser
import parser.expr.Variable
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserFunction.parserFunction

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserFunctionTest extends AnyFunSuite with Matchers {

  test("Parser function with multiple parameters, Should return Function statement") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(COMMA, ",", 1, None),
      Token(IDENTIFIER, "b", 1, Some("b")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(LEFT_BRACE, "{", 1, None),
      Token(PRINT, "print", 2, None),
      Token(IDENTIFIER, "b", 2, Some("b")),
      Token(SEMICOLON, ";", 2, None),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(EOF, "", 4, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Function(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      List(Token(IDENTIFIER, "a", 1, Some("a")), Token(IDENTIFIER, "b", 1, Some("b"))),
      List(Print(Variable(Token(IDENTIFIER, "b", 2, Some("b")))))
    ))
  }

  test("Error function with wrong name, Should return error") {

    val tokenList: List[Token] = List(
      Token(NUMBER, "2", 1, Some("2")),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect function name."))
  }

  test("Error function with missing left parenthesis, Should return error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect '(' after function name."))
  }

  test("Error function with missing right parenthesis, Should return error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect ')' after parameters."))
  }

  test("Error function with missing left brace, Should return error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect '{' before function body."))
  }

  test("Error function with missing right brace in body, Should return error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(LEFT_BRACE, "{", 1, None),
      Token(PRINT, "print", 2, None),
      Token(IDENTIFIER, "a", 2, Some("a")),
      Token(SEMICOLON, ";", 2, None),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect '}' after block."))
  }

  test("Error function with more thant 255 parameters, should return error") {

    def create255Parameters(count: Int = 255): List[Token] = count match {
      case 0 => List(Token(IDENTIFIER, "a", 1, Some("a")))
      case _ => List(Token(IDENTIFIER, "a", 1, Some("a")), Token(COMMA, ",", 1, None)) ::: create255Parameters(count - 1)
    }

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "sum", 1, Some("sum")),
      Token(LEFT_PAREN, "(", 1, None),
    ) ::: create255Parameters() ::: List(
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFunction(parser.blockStmt())(tokenList)

    tokenListResult should have size 4
    grammarResult shouldBe Left(ErrorCompiler(1, "Can't have more than 255 parameters"))
  }

}

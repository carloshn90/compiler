package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{COMMA, DOT, EOF, IDENTIFIER, LEFT_PAREN, NUMBER, PRINT, RIGHT_PAREN, STRING, Token}
import parser.Parser
import parser.expr.ParserCall.parserCall
import parser.grammar.GrammarResult.GrammarResult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserCallTest extends AnyFunSuite with Matchers {

  test("Parsing function call with one argument, should return call statement with one argument") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument1", 1, Some("argument1")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Call(Variable(Token(IDENTIFIER, "functionName", 1, Some("functionName"))),Token(IDENTIFIER, "functionName", 1, Some("functionName")), List(Literal("argument1"))))
  }

  test("Parsing function call with multiples arguments, should return call statement with multiple arguments") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument1", 1, Some("argument1")),
      Token(COMMA, ",", 1, None),
      Token(NUMBER, "4.2", 1, Some(4.2)),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )
    val argumentsExpected: List[Expr] = List(Literal("argument1"), Literal(4.2))

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Call(Variable(Token(IDENTIFIER, "functionName", 1, Some("functionName"))),Token(IDENTIFIER, "functionName", 1, Some("functionName")), argumentsExpected))
  }

  test("parsing class with a method, should return call with get expression") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(DOT, ".", 1, None),
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument1", 1, Some("argument1")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val getExpected: Get = Get(Variable(Token(IDENTIFIER, "className", 1, Some("className"))), Token(IDENTIFIER, "functionName", 1, Some("functionName")))
    val callExpected: Call = Call(getExpected, Token(IDENTIFIER, "functionName", 1, Some("functionName")), List(Literal("argument1")))

    val parser: Parser = new Parser()
    val (result: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 1
    result shouldBe Right(callExpected)
  }

  test("parsing class with multiple chain call, should return call with get expression") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(DOT, ".", 1, None),
      Token(IDENTIFIER, "functionName1", 1, Some("functionName1")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument1", 1, Some("argument1")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(DOT, ".", 1, None),
      Token(IDENTIFIER, "functionName2", 1, Some("functionName2")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument2", 1, Some("argument2")),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val getClassExpected: Get = Get(Variable(Token(IDENTIFIER, "className", 1, Some("className"))), Token(IDENTIFIER, "functionName1", 1, Some("functionName1")))
    val callFunction1Expected: Call = Call(getClassExpected, Token(IDENTIFIER, "functionName1", 1, Some("functionName1")), List(Literal("argument1")))
    val getFunction1Expected: Get = Get(callFunction1Expected, Token(IDENTIFIER, "functionName2", 1, Some("functionName2")))
    val callFunction2Expected: Call = Call(getFunction1Expected, Token(IDENTIFIER, "functionName2", 1, Some("functionName2")), List(Literal("argument2")))


    val parser: Parser = new Parser()
    val (result: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 1
    result shouldBe Right(callFunction2Expected)
  }

  test("Error Parsing function name, should return error") {
    val tokenList: List[Token] = List(
      Token(PRINT, "print", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }

  test("Error Parsing argument, should return error") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(PRINT, "print", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }

  test("Error missing right parenthesis, should return error") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument1", 1, Some("argument1")),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect ')' after arguments."))
  }

  test("Error extract comma, should return error") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None),
      Token(STRING, "argument1", 1, Some("argument1")),
      Token(COMMA, ",", 1, None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 3
    grammarResult shouldBe Left(ErrorCompiler(1, "Extra ',' is not allowed here."))
  }

  test("Error more thant 255 arguments, should return error") {

    def create255Arguments(count: Int = 255): List[Token] = count match {
      case 0 => List(Token(STRING, "argument1", 1, Some("argument1")))
      case _ => List(Token(STRING, "argument1", 1, Some("argument1")), Token(COMMA, ",", 1, None)) ::: create255Arguments(count - 1)
    }

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "functionName", 1, Some("functionName")),
      Token(LEFT_PAREN, "(", 1, None)
    ) ::: create255Arguments() ::: List(
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 4
    grammarResult shouldBe Left(ErrorCompiler(1, "Can't have more than 255 arguments."))
  }

  test("Error not property name after dot, should return error") {
    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(DOT, ".", 1, None),
      Token(STRING, "functionName", 1, Some("functionName")),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserCall(parser.primary(), parser.expression())(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(1, "Expect property name after '.'."))
  }

}

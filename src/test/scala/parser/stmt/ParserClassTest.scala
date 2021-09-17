package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, EQUAL, FUN, IDENTIFIER, LEFT_BRACE, LEFT_PAREN, PRINT, RIGHT_BRACE, RIGHT_PAREN, SEMICOLON, STRING, Token, VAR}
import parser.Parser
import parser.expr.{Literal, Variable}
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserClass.parserClass

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserClassTest extends AnyFunSuite with Matchers {

  test("Parsing class statement, should return a class with a function") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(LEFT_BRACE, "{", 1, None),
      Token(FUN, "fun", 2, None),
      Token(IDENTIFIER, "funName", 2, Some("funName")),
      Token(LEFT_PAREN, "(", 2, None),
      Token(IDENTIFIER, "argName", 2, Some("argName")),
      Token(RIGHT_PAREN, ")", 2, None),
      Token(LEFT_BRACE, "{", 2, None),
      Token(PRINT, "print", 3, None),
      Token(IDENTIFIER, "argName", 3, Some("argName")),
      Token(SEMICOLON, ";", 3, None),
      Token(RIGHT_BRACE, "}", 4, None),
      Token(RIGHT_BRACE, "}", 5, None),
      Token(EOF, "", 6, None)
    )

    val functionExpected: Function = Function(
      Token(IDENTIFIER, "funName", 2, Some("funName")),
      List(Token(IDENTIFIER, "argName", 2, Some("argName"))),
      List(Print(Variable(Token(IDENTIFIER, "argName", 3, Some("argName")))))
    )

    val classExpected: Class = Class(
      Token(IDENTIFIER, "className", 1, Some("className")),
      List(),
      List(functionExpected)
    )

    val parser: Parser = new Parser()
    val (classResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserClass(parser.expression(), parser.funDecl())(tokenList)

    tokenListResult should have size 1
    classResult shouldBe Right(classExpected)
  }

  test("Parsing class statement with a var, should return a class with a var") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(LEFT_BRACE, "{", 1, None),
      Token(VAR, "var", 2, Some("var")),
      Token(IDENTIFIER, "varName", 2, Some("varName")),
      Token(EQUAL, "=", 2, None),
      Token(STRING, "varValue", 2, Some("varValue")),
      Token(SEMICOLON, ";", 2, None),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(EOF, "", 4, None)
    )

    val varExpected: Var = Var(
      Token(IDENTIFIER, "varName", 2, Some("varName")),
      Literal("varValue")
    )

    val classExpected: Class = Class(
      Token(IDENTIFIER, "className", 1, Some("className")),
      List(varExpected),
      List()
    )

    val parser: Parser = new Parser()
    val (classResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserClass(parser.expression(), parser.funDecl())(tokenList)

    tokenListResult should have size 1
    classResult shouldBe Right(classExpected)
  }

  test("Error Parsing class statement with a var without semicolon, should return an error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(LEFT_BRACE, "{", 1, None),
      Token(VAR, "var", 2, Some("var")),
      Token(IDENTIFIER, "varName", 2, Some("varName")),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(EOF, "", 4, None)
    )

    val parser: Parser = new Parser()
    val (classResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserClass(parser.expression(), parser.funDecl())(tokenList)

    tokenListResult should have size 2
    classResult shouldBe Left(ErrorCompiler(2, "Expect ';' after value."))
  }

  test("Parsing class statement missing left brace, should return an error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (classResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserClass(parser.expression(), parser.funDecl())(tokenList)

    tokenListResult should have size 1
    classResult shouldBe Left(ErrorCompiler(1, "Expect '{' before class body."))
  }

  test("Parsing class statement missing right brace, should return an error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(LEFT_BRACE, "{", 1, None),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (classResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserClass(parser.expression(), parser.funDecl())(tokenList)

    tokenListResult should have size 1
    classResult shouldBe Left(ErrorCompiler(2, "Expect '}' after class body."))
  }

  test("Parsing class statement error parsing function, should return an error") {

    val tokenList: List[Token] = List(
      Token(IDENTIFIER, "className", 1, Some("className")),
      Token(LEFT_BRACE, "{", 1, None),
      Token(FUN, "fun", 2, None),
      Token(IDENTIFIER, "funName", 2, Some("funName")),
      Token(RIGHT_BRACE, "}", 3, None),
      Token(EOF, "", 4, None)
    )

    val parser: Parser = new Parser()
    val (classResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserClass(parser.expression(), parser.funDecl())(tokenList)

    tokenListResult should have size 2
    classResult shouldBe Left(ErrorCompiler(3, "Expect '(' after function name."))
  }

}

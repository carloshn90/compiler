package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, EQUAL, IDENTIFIER, LEFT_PAREN, LESS, NUMBER, PLUS, PRINT, RIGHT_PAREN, SEMICOLON, Token, VAR}
import parser.Parser
import parser.expr.{Assign, Binary, Expr, Literal, Variable}
import parser.grammar.GrammarResult.GrammarResult
import parser.stmt.ParserFor.parserFor

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserForTest extends AnyFunSuite with Matchers {

  val aVarToken: Token = Token(IDENTIFIER, "a", 1, Some("a"))
  val variableExpected: Expr = Variable(aVarToken)
  val incrementBinary: Expr = Binary(variableExpected, Token(PLUS, "+", 1, None), Literal(1))
  val initializerExpected: Stmt = Var(aVarToken, Literal(2.4))
  val condExpected: Expr = Binary(Literal(1),Token(LESS, "<", 1, None), variableExpected)
  val incrementExpected: Stmt = Expression(Assign(aVarToken, incrementBinary))
  val bodyExpected: Stmt = Print(variableExpected)

  test("ParserFor initializer with Var declaration, Should return var") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(VAR, "var", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(LESS, "<", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(PLUS, "+", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )
    val whileExpected: Stmt = While(condExpected, Block(List(bodyExpected, incrementExpected)))

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Block(List(initializerExpected, whileExpected)))
  }

  test("ParserFor initializer with expression, Should return expression") {

    val assignExpected: Stmt = Expression(Assign(Token(IDENTIFIER, "a", 1, None), Literal(2.4)))

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, None),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(LESS, "<", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(PLUS, "+", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )
    val whileExpected: Stmt = While(condExpected, Block(List(bodyExpected, incrementExpected)))

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Block(List(assignExpected, whileExpected)))
  }

  test("ParserFor initializer with semicolon, Should return while with out initializer") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(SEMICOLON, ";", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(LESS, "<", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(PLUS, "+", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )
    val whileExpected: Stmt = While(condExpected, Block(List(bodyExpected, incrementExpected)))

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(whileExpected)
  }

  test("Only initializer, Statement with only initializer section") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(VAR, "var", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(SEMICOLON, ";", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(PLUS, "+", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )
    val whileExpected: Stmt = While(Literal(true), Block(List(bodyExpected, incrementExpected)))

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Block(List(initializerExpected, whileExpected)))
  }

  test("For parser without increment section, For statement without increment") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(VAR, "var", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(LESS, "<", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(PRINT, "print", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )
    val whileExpected: Stmt = While(condExpected, bodyExpected)

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Block(List(initializerExpected, whileExpected)))
  }

  test("Error parserFor initializer, Should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, None),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(EOF, "", 3, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect ';' after value."))
  }

  test("Error malformed condition for, Should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(IDENTIFIER, "a", 1, None),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Malformed for expression"))
  }

  test("Error missing semicolon after condition, Should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(VAR, "var", 1, None),
      Token(IDENTIFIER, "a", 1, None),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(LESS, "<", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect ';' after loop condition." ))
  }

  test("Error missing right parenthesis after increment, Should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(VAR, "var", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(SEMICOLON, ";", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(PLUS, "+", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "Expect ')' after for clauses."))
  }

  test("Error missing body, Should return error") {

    val tokenList: List[Token] = List(
      Token(LEFT_PAREN, "(", 1, None),
      Token(VAR, "var", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(NUMBER, "2.4", 1, Some(2.4)),
      Token(SEMICOLON, ";", 1, None),
      Token(SEMICOLON, ";", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(EQUAL, "=", 1, None),
      Token(IDENTIFIER, "a", 1, Some("a")),
      Token(PLUS, "+", 1, None),
      Token(NUMBER, "1", 1, Some(1)),
      Token(RIGHT_PAREN, ")", 1, None),
      Token(EOF, "", 2, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Stmt], tokenListResult: List[Token]) = parserFor(
      parser.varDecl(), parser.exprStmt(),
      parser.expression(), parser.statement()
    )(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Left(ErrorCompiler(2, "the expression '' is not allowed here."))
  }
}

package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{AND, EOF, FALSE, OR, PRINT, TRUE, Token}
import parser.Parser
import parser.expr.ParserLogical.parserLogical
import parser.grammar.GrammarResult.GrammarResult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserLogicalTest extends AnyFunSuite with Matchers {

  test("Parsing OR expression, should return logical expression") {
    val tokenList: List[Token] = List(
      Token(TRUE, "true", 1, None),
      Token(OR, "or", 1, None),
      Token(FALSE, "false", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLogical(parser.logicAnd(), OR)(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Logical(Literal(true), Token(OR, "or", 1, None), Literal(false)))
  }

  test("Parsing  multiples OR expressions, should return all logical expressions") {
    val tokenList: List[Token] = List(
      Token(TRUE, "true", 1, None),
      Token(OR, "or", 1, None),
      Token(FALSE, "false", 1, None),
      Token(OR, "or", 1, None),
      Token(TRUE, "true", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLogical(parser.logicAnd(), OR)(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Logical(Logical(Literal(true), Token(OR, "or", 1, None), Literal(false)), Token(OR, "or", 1, None), Literal(true)))
  }

  test("Parsing AND expression, should return logical expression") {
    val tokenList: List[Token] = List(
      Token(TRUE, "true", 1, None),
      Token(AND, "and", 1, None),
      Token(FALSE, "false", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLogical(parser.equality(), AND)(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Logical(Literal(true), Token(AND, "and", 1, None), Literal(false)))
  }

  test("Parsing  multiples combine OR|AND expressions, should return all logical expressions") {
    val tokenList: List[Token] = List(
      Token(TRUE, "true", 1, None),
      Token(OR, "or", 1, None),
      Token(FALSE, "false", 1, None),
      Token(AND, "and", 1, None),
      Token(TRUE, "true", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLogical(parser.logicAnd(), OR)(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Logical(Literal(true), Token(OR, "or", 1, None), Logical(Literal(false), Token(AND, "and", 1, None), Literal(true))))
  }

  test("Error Parsing left expression, should return error") {
    val tokenList: List[Token] = List(
      Token(PRINT, "print", 1, None),
      Token(OR, "or", 1, None),
      Token(FALSE, "false", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLogical(parser.logicAnd(), OR)(tokenList)

    tokenListResult should have size 4
    grammarResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }

  test("Error Parsing right expression, should return error") {
    val tokenList: List[Token] = List(
      Token(TRUE, "true", 1, None),
      Token(OR, "or", 1, None),
      Token(PRINT, "print", 1, None),
      Token(EOF, "", 1, None)
    )

    val parser: Parser = new Parser()
    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserLogical(parser.logicAnd(), OR)(tokenList)

    tokenListResult should have size 2
    grammarResult shouldBe Left(ErrorCompiler(1, "the expression 'print' is not allowed here."))
  }
}

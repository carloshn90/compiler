package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.stmt.InterVar.interVar
import lexer.{IDENTIFIER, Token}
import parser.expr.{Expr, Literal, Variable}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterVarTest extends AnyFunSuite with Matchers {

  test("Interpreting var, should return environment with token (identifier) and value") {

    val token: Token = Token(IDENTIFIER, "var name", 0, Some("var name"))
    val expr: Expr = Literal(Nil)
    val env: Environment = new Environment()

    val result: Environment = interVar(token, expr)(env)._2

    result.size shouldBe 1
    result.get(token.lexeme) shouldBe Right(Nil)
  }

  test("Interpreting var override value, should return last value") {

    val token: Token = Token(IDENTIFIER, "var name", 0, Some("var name"))
    val expr: Expr = Literal(Nil)
    val overrideExpr: Expr = Literal(2.0)
    val env: Environment = new Environment()

    val result: Environment = interVar(token, expr)(env)._2
    val overrideResult: Environment = interVar(token, overrideExpr)(result)._2

    overrideResult.size shouldBe 1
    overrideResult.get(token.lexeme) shouldBe Right(2.0)
  }

  test("Interpreting var add multiples values, should return both values") {

    val firstToken: Token = Token(IDENTIFIER, "a", 0, Some("b"))
    val secondToken: Token = Token(IDENTIFIER, "b", 0, Some("b"))
    val firstExpr: Expr = Literal(Nil)
    val secondExpr: Expr = Literal(2.0)
    val env: Environment = new Environment()

    val result: Environment = interVar(firstToken, firstExpr)(env)._2
    val bothValuesResult: Environment = interVar(secondToken, secondExpr)(result)._2

    bothValuesResult.size shouldBe 2
    bothValuesResult.get(firstToken.lexeme) shouldBe Right(Nil)
    bothValuesResult.get(secondToken.lexeme) shouldBe Right(2.0)
  }

  test("Interpreting var with error, should return error") {

    val token: Token = Token(IDENTIFIER, "a", 0, Some("b"))
    val expr: Expr = Variable(token)
    val env: Environment = new Environment()

    val (exprResult: Either[ErrorCompiler, Option[String]], envResult: Environment) = interVar(token, expr)(env)

    envResult.size shouldBe 0
    exprResult shouldBe Left(ErrorCompiler(0, "Undefined variable 'a'."))
  }
}

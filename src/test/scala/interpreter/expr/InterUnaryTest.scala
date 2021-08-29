package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.expr.InterUnary.interUnary
import interpreter.{Environment, InterpreterState, Result}
import lexer.{BANG, MINUS, Token}
import parser.expr.{Expr, Literal}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterUnaryTest extends AnyFunSuite with Matchers {

  test("Interpreting Minus Unary expression, should return the minus value") {

    val token: Token = Token(MINUS, "-", 0, None)
    val expr: Expr = Literal(1.0)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interUnary(token, expr)(env)._1

    result shouldBe Right(InterpreterState(List(), Some(-1.0)))
  }

  test("Interpreting Bang Unary expression, should return the opposite boolean value") {

    val token: Token = Token(BANG, "!", 0, None)
    val expr: Expr = Literal(true)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interUnary(token, expr)(env)._1

    result shouldBe Right(InterpreterState(List(), Some(false)))
  }

  test("Interpreting Bang Unary expression string literal, should return the opposite boolean value") {

    val token: Token = Token(BANG, "!", 0, None)
    val expr: Expr = Literal("Hi")
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interUnary(token, expr)(env)._1

    result shouldBe Right(InterpreterState(List(), Some(false)))
  }

}

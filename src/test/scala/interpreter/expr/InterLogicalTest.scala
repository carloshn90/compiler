package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.Environment
import interpreter.expr.InterLogical.interLogical
import lexer.{AND, IDENTIFIER, OR, Token}
import parser.expr.{Expr, Literal, Variable}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterLogicalTest extends AnyFunSuite with Matchers {

  case class LogicalCase(leftValue: Any, operator: Token, rightValue: Any, expected: Any)

  private val logicalCases: Seq[LogicalCase] = Seq(
    LogicalCase(true, Token(OR, "or", 0, None), true, true),
    LogicalCase(false, Token(OR, "or", 0, None), true, true),
    LogicalCase(true, Token(OR, "or", 0, None), false, true),
    LogicalCase(false, Token(OR, "or", 0, None), false, false),
    LogicalCase(true, Token(AND, "and", 0, None), true, true),
    LogicalCase(false, Token(AND, "and", 0, None), true, false),
    LogicalCase(true, Token(AND, "and", 0, None), false, false),
    LogicalCase(false, Token(AND, "and", 0, None), false, false),
  )
  for (LogicalCase(leftValue, operator, rightValue, expected) <- logicalCases) {
    test(s"Logical interpreter $leftValue ${operator.lexeme} $rightValue, should be equal to $expected") {
      val left: Expr = Literal(leftValue)
      val right: Expr = Literal(rightValue)
      val env: Environment = new Environment()

      val result: Either[ErrorCompiler, Any] = interLogical(left, operator, right)(env)._1

      result shouldBe Right(expected)
    }
  }

  test("Error in left expression in logical interpreter, should return error") {
    val left: Expr = Variable(Token(IDENTIFIER, "a", 1, Some("a")))
    val operator: Token = Token(AND, "and", 0, None)
    val right: Expr = Literal(true)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Any] = interLogical(left, operator, right)(env)._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }

  test("Error in right expression in logical interpreter, should return error") {
    val left: Expr = Literal(false)
    val operator: Token = Token(OR, "or", 0, None)
    val right: Expr = Variable(Token(IDENTIFIER, "a", 1, Some("a")))
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Any] = interLogical(left, operator, right)(env)._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }

  test("And no execute right expression, should return left value") {
    val left: Expr = Literal(false)
    val operator: Token = Token(AND, "and", 0, None)
    val right: Expr = Variable(Token(IDENTIFIER, "a", 1, Some("a")))
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Any] = interLogical(left, operator, right)(env)._1

    result shouldBe Right(false)
  }

  test("Or no execute right expression, should return left value") {
    val left: Expr = Literal(true)
    val operator: Token = Token(OR, "or", 0, None)
    val right: Expr = Variable(Token(IDENTIFIER, "a", 1, Some("a")))
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Any] = interLogical(left, operator, right)(env)._1

    result shouldBe Right(true)
  }
}

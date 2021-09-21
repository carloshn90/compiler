package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.{Environment, InterpreterState, Result}
import interpreter.expr.InterBinary.interBinary
import lexer.{BANG_EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, MINUS, MODULE, PLUS, SLASH, STAR, Token}
import parser.expr.{Expr, Literal}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterBinaryTest extends AnyFunSuite with Matchers {

  case class BinaryCase(leftValue: Any, operator: Token, rightValue: Any, expected: Either[ErrorCompiler, Any])

  private val binaryCases: Seq[BinaryCase] = Seq(
    BinaryCase(1.14, Token(PLUS, "+", 0, None), 2.28, Right(3.42)),
    BinaryCase("Hello ", Token(PLUS, "+", 0, None), "world", Right("Hello world")),
    BinaryCase(2.0, Token(PLUS, "+", 0, None), " World", Right("2.0 World")),
    BinaryCase("Hello ", Token(PLUS, "+", 0, None), 2.0, Right("Hello 2.0")),
    BinaryCase("Hello", Token(PLUS, "+", 0, None), List(2.0), Left(ErrorCompiler(0, s"It isn't possible to add these values: Hello + List(2.0)"))),
    BinaryCase(6.0, Token(SLASH, "/", 0, None), 2.0, Right(3.0)),
    BinaryCase(3.0, Token(SLASH, "/", 0, None), 0.0, Right(Double.PositiveInfinity)),
    BinaryCase(6.0, Token(STAR, "*", 0, None), 2.0, Right(12.0)),
    BinaryCase(2.0, Token(MODULE, "%", 0, None), 2.0, Right(0.0)),
    BinaryCase(3.0, Token(MODULE, "%", 0, None), 2.0, Right(1.0)),
    BinaryCase(6.0, Token(MINUS, "-", 0, None), 2.0, Right(4.0)),
    BinaryCase(6.0, Token(GREATER, ">", 0, None), 2.0, Right(true)),
    BinaryCase(6.0, Token(GREATER, ">", 0, None), 6.0, Right(false)),
    BinaryCase(6.0, Token(GREATER, ">", 0, None), 7.0, Right(false)),
    BinaryCase(6.0, Token(GREATER_EQUAL, ">=", 0, None), 2.0, Right(true)),
    BinaryCase(6.0, Token(GREATER_EQUAL, ">=", 0, None), 6.0, Right(true)),
    BinaryCase(6.0, Token(GREATER_EQUAL, ">=", 0, None), 7.0, Right(false)),
    BinaryCase(6.0, Token(LESS, "<", 0, None), 2.0, Right(false)),
    BinaryCase(6.0, Token(LESS, "<", 0, None), 6.0, Right(false)),
    BinaryCase(6.0, Token(LESS, "<", 0, None), 7.0, Right(true)),
    BinaryCase(6.0, Token(LESS_EQUAL, "<=", 0, None), 2.0, Right(false)),
    BinaryCase(6.0, Token(LESS_EQUAL, "<=", 0, None), 6.0, Right(true)),
    BinaryCase(6.0, Token(LESS_EQUAL, "<=", 0, None), 7.0, Right(true)),
    BinaryCase(6.0, Token(BANG_EQUAL, "!=", 0, None), 7.0, Right(true)),
    BinaryCase(6.0, Token(BANG_EQUAL, "!=", 0, None), 6.0, Right(false)),
    BinaryCase(true, Token(BANG_EQUAL, "!=", 0, None), false, Right(true)),
    BinaryCase(false, Token(BANG_EQUAL, "!=", 0, None), false, Right(false)),
    BinaryCase(6.0, Token(EQUAL_EQUAL, "==", 0, None), 7.0, Right(false)),
    BinaryCase(6.0, Token(EQUAL_EQUAL, "==", 0, None), 6.0, Right(true)),
    BinaryCase(true, Token(EQUAL_EQUAL, "==", 0, None), false, Right(false)),
    BinaryCase(false, Token(EQUAL_EQUAL, "==", 0, None), false, Right(true)),
  )

  for (BinaryCase(leftValue, operator, rightValue, expected) <- binaryCases) {
    test(s"Binary interpreter $leftValue ${operator.lexeme} $rightValue, should be equal to $expected") {
      val left: Expr = Literal(leftValue)
      val right: Expr = Literal(rightValue)
      val env: Environment = new Environment()

      val result: Either[ErrorCompiler, Result] = interBinary(left, operator, right)(env)._1

      result shouldBe expected.map(e => InterpreterState(List(), Some(e)))
    }
  }
}

package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.stmt.InterReturn.interReturn
import interpreter.{Environment, Result, ReturnResult}
import lexer.{IDENTIFIER, Token}
import parser.expr.{Assign, Expr, Literal}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterReturnTest extends AnyFunSuite with Matchers {

  test("Interpreter None expression, should return result empty") {

    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interReturn(Expr.None)(env)._1

    result shouldBe Right(ReturnResult(List(), None))
  }

  test("Interpreter correct expression, should return value") {

    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interReturn(Literal(1))(env)._1

    result shouldBe Right(ReturnResult(List(), Some(1)))
  }

  test("Interpreter wrong expression, should return error") {

    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interReturn(Assign(Token(IDENTIFIER, "a", 1, Some("a")), Literal(1)))(env)._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }
}

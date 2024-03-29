package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import helper.TestEnvironmentHelper.defineOrFail
import interpreter.expr.InterAssign.interAssign
import interpreter.{Environment, InterpreterState, Result}
import lexer.{IDENTIFIER, PLUS, Token}
import parser.expr.{Binary, Expr, Literal}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterAssignTest extends AnyFunSuite with Matchers {

  test("Interpreting assign variable present in environment, should modify value") {

    val varName: Token = Token(IDENTIFIER, "var name", 0, Some("var name"))
    val oldValue: Double = 3.455
    val newValueExpr: Expr =  Literal("hello")
    val env: Environment = defineOrFail(new Environment(), varName, oldValue)

    val (resultValue: Either[ErrorCompiler, Result], resultEnv: Environment) = interAssign(varName, newValueExpr)(env)

    resultEnv.size shouldBe 1
    resultValue shouldBe Right(InterpreterState(List(), Some("hello")))
  }

  test("Interpreting assign variable no present in environment, should return error") {

    val varName: Token = Token(IDENTIFIER, "var name", 10, Some("var name"))
    val valueExpr: Expr =  Literal("hello")
    val env: Environment = new Environment()

    val (resultValue: Either[ErrorCompiler, Result], resultEnv: Environment) = interAssign(varName, valueExpr)(env)

    resultEnv.size shouldBe 0
    resultValue shouldBe Left(ErrorCompiler(10, "Undefined variable 'var name'."))
  }

  test("Interpreting assign variable error evaluating expression, should return error") {

    val varName: Token = Token(IDENTIFIER, "var name", 10, Some("var name"))
    val wrongExpr: Expr =  Binary(Literal("hello"), Token(PLUS, "+", 3, None), Literal(List(2.0)))
    val env: Environment = new Environment()

    val (resultValue: Either[ErrorCompiler, Result], resultEnv: Environment) = interAssign(varName, wrongExpr)(env)

    resultEnv.size shouldBe 0
    resultValue shouldBe Left(ErrorCompiler(3, s"It isn't possible to add these values: hello + List(2.0)"))
  }

}

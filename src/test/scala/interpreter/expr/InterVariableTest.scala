package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.expr.InterVariable.interVariable
import interpreter.{Environment, InterpreterState, Result}
import lexer.{IDENTIFIER, STRING, Token}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterVariableTest extends AnyFunSuite with Matchers {

  test("Interpreting variable present in environment, should return value") {

    val varName: Token = Token(IDENTIFIER, "var name", 0, Some("var name"))
    val varValue: Token = Token(STRING, "Some string value", 0, Some("Some string value"))
    val env: Environment = new Environment().define(varName.lexeme, varValue)

    val result: Either[ErrorCompiler, Result] = interVariable(varName)(env)._1

    result shouldBe Right(InterpreterState(List(), Some(varValue)))
  }

  test("Interpreting variable no present in environment, should return error") {

    val varName: Token = Token(IDENTIFIER, "var name", 0, Some("var name"))
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interVariable(varName)(env)._1

    result shouldBe Left(ErrorCompiler(0, "Undefined variable 'var name'."))
  }

}

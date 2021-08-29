package org.compiler.example
package interpreter.function

import error.ErrorCompiler
import interpreter.{Environment, InterpreterState, Result}
import lexer.{IDENTIFIER, PLUS, Token}
import parser.expr.{Binary, Literal, Variable}
import parser.stmt.{Function, Print, Stmt}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterFunctionTest extends AnyFunSuite with Matchers {

  test("InterFunction with parameters should return string") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val params: List[Token] = List(Token(IDENTIFIER, "a", 1, Some("a")))
    val body: List[Stmt] = List(
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val function: Function = Function(funName, params, body)
    val interFunction: InterFunction = new InterFunction(function)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interFunction.call(List(InterpreterState(List(), Some("Hello"))))(env)._1

    result shouldBe Right(InterpreterState(List("Hello"), None))
  }

  test("Error in the body statement should return an error") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val params: List[Token] = List(Token(IDENTIFIER, "a", 1, Some("a")))
    val body: List[Stmt] = List(
      Print(Binary(Literal(2.0), Token(PLUS, "+", 1, None), Literal(List())))
    )

    val function: Function = Function(funName, params, body)
    val interFunction: InterFunction = new InterFunction(function)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Result] = interFunction.call(List(InterpreterState(List(), Some("Hello"))))(env)._1

    result shouldBe Left(ErrorCompiler(1, "It isn't possible to add these values: 2.0 + List()"))
  }

}

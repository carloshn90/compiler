package org.compiler.example
package interpreter.function

import error.ErrorCompiler
import helper.TestEnvironmentHelper.defineOrFail
import interpreter.{Environment, InterpreterState, Result}
import lexer.{IDENTIFIER, PLUS, Token}
import parser.expr.{Assign, Binary, Literal, Variable}
import parser.stmt.{Expression, Function, Print, Stmt}

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
    val env: Environment = new Environment()
    val interFunction: InterFunction = new InterFunction(function, env)

    val result: Either[ErrorCompiler, Result] = interFunction.call(funName, List(InterpreterState(List(), Some("Hello"))))(env)._1

    result shouldBe Right(InterpreterState(List("Hello"), None))
  }

  test("InterFunction modify global environment should return global environment modified") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val params: List[Token] = List()
    val body: List[Stmt] = List(
      Expression(Assign(Token(IDENTIFIER, "a", 1, Some("a")), Literal(1))),
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val function: Function = Function(funName, params, body)
    val interFunction: InterFunction = new InterFunction(function, new Environment())
    val env: Environment = defineOrFail(defineOrFail(new Environment(), Token(IDENTIFIER, "a", 1, Some("a")), 0), Token(IDENTIFIER, "sum", 1, Some("sum")), interFunction)

    val (result: Either[ErrorCompiler, Result], resultEnv: Environment) = interFunction.call(funName, List(InterpreterState(List(), Some("Hello"))))(env)


    resultEnv.size shouldBe 2
    resultEnv.get("a") shouldBe Right(1)
    result shouldBe Right(InterpreterState(List("1"), None))
  }

  test("InterFunction with arguments should return clean environment") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val param1: Token = Token(IDENTIFIER, "param1", 1, Some("param1"))
    val param2: Token = Token(IDENTIFIER, "param2", 1, Some("param2"))
    val params: List[Token] = List(param1, param2)
    val body: List[Stmt] = List(
      Expression(Assign(Token(IDENTIFIER, "a", 1, Some("a")), Literal(1))),
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val function: Function = Function(funName, params, body)
    val interFunction: InterFunction = new InterFunction(function, new Environment())
    val deepEnv: Environment = defineOrFail(defineOrFail(new Environment(), Token(IDENTIFIER, "a", 1, Some("a")), 0), Token(IDENTIFIER, "sum", 1, Some("sum")), interFunction)
    val env: Environment = new Environment(List(Map[String, Any]()) ::: deepEnv.getValues)

    val callParams: List[Result] = List(
      InterpreterState(List(), Some(Literal("param1"))),
      InterpreterState(List(), Some(Literal("param2")))
    )
    val resultEnv: Environment = interFunction.call(funName, callParams)(env)._2

    resultEnv.getValues.length shouldBe 2
  }

  test("InterFunction without arguments should return clean environment") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val body: List[Stmt] = List(
      Expression(Assign(Token(IDENTIFIER, "a", 1, Some("a")), Literal(1))),
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val function: Function = Function(funName, List(), body)
    val interFunction: InterFunction = new InterFunction(function, new Environment())
    val deepEnv: Environment = defineOrFail(defineOrFail(new Environment(), Token(IDENTIFIER, "a", 1, Some("a")), 0), Token(IDENTIFIER, "sum", 1, Some("sum")), interFunction)
    val env: Environment = new Environment(List(Map[String, Any]()) ::: deepEnv.getValues)

    val resultEnv: Environment = interFunction.call(funName, List())(env)._2

    resultEnv.getValues.length shouldBe 2
  }

  test("Error in the body statement should return an error") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val params: List[Token] = List(Token(IDENTIFIER, "a", 1, Some("a")))
    val body: List[Stmt] = List(
      Print(Binary(Literal(2.0), Token(PLUS, "+", 1, None), Literal(List())))
    )

    val function: Function = Function(funName, params, body)
    val env: Environment = new Environment()
    val interFunction: InterFunction = new InterFunction(function, env)

    val result: Either[ErrorCompiler, Result] = interFunction.call(funName, List(InterpreterState(List(), Some("Hello"))))(env)._1

    result shouldBe Left(ErrorCompiler(1, "It isn't possible to add these values: 2.0 + List()"))
  }

}

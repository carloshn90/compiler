package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.Environment
import interpreter.expr.InterCall.interCall
import interpreter.function.Callable
import lexer.{IDENTIFIER, RIGHT_PAREN, Token}
import parser.expr.{Expr, Literal, Variable}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class TestCaller extends Callable {

  override def argumentSize: Int = 3

  override def call(arguments: List[Any]): Either[String, Any] = {
    val init: Either[String, Double] = Right(0d)
    arguments.foldRight(init)((h: Any, t: Either[String, Double]) => t.flatMap(tt => addArguments(h, tt)))
  }

  private def addArguments(arg: Any, carry: Double): Either[String, Double] = try {
    Right(arg.toString.toDouble + carry)
  } catch {
    case e: Throwable => Left(e.getMessage)
  }
}

class InterCallTest extends AnyFunSuite with Matchers {

  test("Interpreting call undefined function, should return an error") {

    val funExpr: Expr = Variable(Token(IDENTIFIER, "funName", 1, Some("funName")))
    val token: Token = Token(RIGHT_PAREN, ")", 1, None)
    val arguments: List[Expr] = List(Literal(true))
    val env: Environment = new Environment()

    val (resultValue: Either[ErrorCompiler, Any], resultEnv: Environment) = interCall(funExpr, token, arguments)(env)

    resultEnv.size shouldBe 0
    resultValue shouldBe Left(ErrorCompiler(1, "Undefined variable 'funName'."))
  }

  test("Interpreting call error type function, should return an error") {

    val funExpr: Expr = Variable(Token(IDENTIFIER, "funName", 1, Some("funName")))
    val token: Token = Token(RIGHT_PAREN, ")", 1, None)
    val arguments: List[Expr] = List(Literal(true))
    val env: Environment = new Environment().define("funName", 2.2)

    val (resultValue: Either[ErrorCompiler, Any], resultEnv: Environment) = interCall(funExpr, token, arguments)(env)

    resultEnv.size shouldBe 1
    resultValue shouldBe Left(ErrorCompiler(1, "Invalid function type"))
  }

  test("Interpreting call wrong number of arguments, should return an error") {

    val funExpr: Expr = Variable(Token(IDENTIFIER, "funName", 1, Some("funName")))
    val token: Token = Token(RIGHT_PAREN, ")", 1, None)
    val arguments: List[Expr] = List(Literal(true), Literal("Hello"))
    val env: Environment = new Environment().define("funName", new TestCaller())

    val (resultValue: Either[ErrorCompiler, Any], resultEnv: Environment) = interCall(funExpr, token, arguments)(env)

    resultEnv.size shouldBe 1
    resultValue shouldBe Left(ErrorCompiler(1, "Expected 3 arguments but got 2."))
  }

  test("Interpreting call function and add arguments, should return the total sum") {

    val funExpr: Expr = Variable(Token(IDENTIFIER, "funName", 1, Some("funName")))
    val token: Token = Token(RIGHT_PAREN, ")", 1, None)
    val arguments: List[Expr] = List(Literal(2.0), Literal(3.0), Literal(5.0))
    val env: Environment = new Environment().define("funName", new TestCaller())

    val (resultValue: Either[ErrorCompiler, Any], resultEnv: Environment) = interCall(funExpr, token, arguments)(env)

    resultEnv.size shouldBe 1
    resultValue shouldBe Right(10)
  }

  test("Interpreting call function error inside function, should return error") {

    val funExpr: Expr = Variable(Token(IDENTIFIER, "funName", 1, Some("funName")))
    val token: Token = Token(RIGHT_PAREN, ")", 1, None)
    val arguments: List[Expr] = List(Literal(2.0), Literal(3.0), Literal("hello"))
    val env: Environment = new Environment().define("funName", new TestCaller())

    val (resultValue: Either[ErrorCompiler, Any], resultEnv: Environment) = interCall(funExpr, token, arguments)(env)

    resultEnv.size shouldBe 1
    resultValue shouldBe Left(ErrorCompiler(1, "For input string: \"hello\""))
  }
}

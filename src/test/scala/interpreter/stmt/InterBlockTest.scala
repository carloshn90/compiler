package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.stmt.InterBlock.interBlock
import lexer.{IDENTIFIER, PLUS, Token}
import parser.expr.{Assign, Binary, Literal, Variable}
import parser.stmt.{Expression, Print, Stmt, Var}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterBlockTest extends AnyFunSuite with Matchers {

  test("Interpreting Block with two prints, should return two strings") {

    val blocks: List[Stmt] = List(
      Print(Literal("Hello")),
      Print(Literal(2.33)),
    )

    val result: Either[ErrorCompiler, List[String]] = interBlock(blocks)(new Environment())._1

    result shouldBe Right(List("Hello", "2.33"))
  }

  test("Interpreting Block should return original environment") {

    val blocks: List[Stmt] = List(
      Var(Token(IDENTIFIER, "a", 1, Some("a")), Literal(4.0)),
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val originalEnv: Environment = new Environment().define("b", 2.0)
    val (result: Either[ErrorCompiler, List[String]], resultEnv: Environment) = interBlock(blocks)(originalEnv)

    resultEnv shouldBe originalEnv
    result shouldBe Right(List("4"))
  }

  test("Interpreting Block variable defined in a global scope, should return global value") {

    val blocks: List[Stmt] = List(
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val originalEnv: Environment = new Environment().define("a", 4.0)
    val (result: Either[ErrorCompiler, List[String]], resultEnv: Environment) = interBlock(blocks)(originalEnv)

    resultEnv shouldBe originalEnv
    result shouldBe Right(List("4"))
  }

  test("Interpreting error processing statement should return error") {

    val blocks: List[Stmt] = List(
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val result: Either[ErrorCompiler, List[String]] = interBlock(blocks)(new Environment())._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }

  test("Interpreting Block modify global var, should return global var with the new value") {

    val aVar: Token = Token(IDENTIFIER, "a", 1, Some("a"))
    val blocks: List[Stmt] = List(
      Expression(Assign(aVar, Binary(Variable(Token(IDENTIFIER, "a", 3, Some("a"))), Token(PLUS, "+", 3, None), Literal(1.0))))
    )
    val environment: Environment = new Environment().define("a", 0.0)

    val envResult: Environment = interBlock(blocks)(environment)._2

    envResult.get(aVar.lexeme) shouldBe Right(1.0)
  }

}

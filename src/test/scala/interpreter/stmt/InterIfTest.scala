package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.stmt.InterIf.interIf
import lexer.{IDENTIFIER, Token}
import parser.expr.{Expr, Literal, Variable}
import parser.stmt.{Print, Stmt}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterIfTest extends AnyFunSuite with Matchers {

  test("Interpreting if statement true, should print value") {

    val condition: Expr = Literal(true)
    val thenBranch: Stmt = Print(Literal("print"))
    val elseBranch: Option[Stmt] = None

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Right(List("print"))
  }

  test("Interpreting if statement false, shouldn't print value") {

    val condition: Expr = Literal(false)
    val thenBranch: Stmt = Print(Literal("print"))
    val elseBranch: Option[Stmt] = None

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Right(List())
  }

  test("Interpreting if-else statement true, should print if value") {

    val condition: Expr = Literal(true)
    val thenBranch: Stmt = Print(Literal("print if"))
    val elseBranch: Option[Stmt] = Some(Print(Literal("print else")))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Right(List("print if"))
  }

  test("Interpreting if-else statement false, should print else value") {

    val condition: Expr = Literal(false)
    val thenBranch: Stmt = Print(Literal("print if"))
    val elseBranch: Option[Stmt] = Some(Print(Literal("print else")))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Right(List("print else"))
  }

  test("Interpreting if-else error in condition, should return error") {

    val condition: Expr = Variable(Token(IDENTIFIER, "a", 1, Some("a")))
    val thenBranch: Stmt = Print(Literal("print if"))
    val elseBranch: Option[Stmt] = Some(Print(Literal("print else")))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }

  test("Interpreting if-else error in else branch no affect thenBranch, should return else value") {

    val condition: Expr = Literal(true)
    val thenBranch: Stmt = Print(Literal("print if"))
    val elseBranch: Option[Stmt] = Some(Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Right(List("print if"))
  }

  test("Interpreting if-else error in then branch, should return error") {

    val condition: Expr = Literal(true)
    val thenBranch: Stmt = Print(Variable(Token(IDENTIFIER, "a", 1, Some("a"))))
    val elseBranch: Option[Stmt] = Some(Print(Literal("print else")))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }

  test("Interpreting if-else error in then branch no affect elseBranch, should return else value") {

    val condition: Expr = Literal(false)
    val thenBranch: Stmt = Print(Variable(Token(IDENTIFIER, "a", 1, Some("a"))))
    val elseBranch: Option[Stmt] = Some(Print(Literal("print else")))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Right(List("print else"))
  }

  test("Interpreting if-else error in else branch, should return error") {

    val condition: Expr = Literal(false)
    val thenBranch: Stmt = Print(Literal("print if"))
    val elseBranch: Option[Stmt] = Some(Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))))

    val result: Either[ErrorCompiler, List[String]] = interIf(condition, thenBranch, elseBranch)(new Environment())._1

    result shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }
}

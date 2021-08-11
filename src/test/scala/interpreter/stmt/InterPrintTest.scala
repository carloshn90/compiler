package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.stmt.InterPrint.interPrint
import parser.expr.{Expr, Literal}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterPrintTest extends AnyFunSuite with Matchers {

  test("Interpreting print value Nil, should return string 'Nil'") {

    val expr: Expr = Literal(Nil)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Option[String]] = interPrint(expr)(env)._1

    result shouldBe Right(Some("Nil"))
  }

  test("Interpreting print value null, should return string 'Nil'") {

    val expr: Expr = Literal(null)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Option[String]] = interPrint(expr)(env)._1

    result shouldBe Right(Some("Nil"))
  }

  test("Interpreting print value double with decimal .0, should return string without decimal side") {

    val expr: Expr = Literal(1.0)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Option[String]] = interPrint(expr)(env)._1

    result shouldBe Right(Some("1"))
  }

  test("Interpreting print value double with decimals, should return string with decimal") {

    val expr: Expr = Literal(3.14)
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Option[String]] = interPrint(expr)(env)._1

    result shouldBe Right(Some("3.14"))
  }

  test("Interpreting print other value, should return string with the value") {

    val expr: Expr = Literal("nullable")
    val env: Environment = new Environment()

    val result: Either[ErrorCompiler, Option[String]] = interPrint(expr)(env)._1

    result shouldBe Right(Some("nullable"))
  }
}

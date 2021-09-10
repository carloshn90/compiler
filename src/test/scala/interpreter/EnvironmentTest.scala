package org.compiler.example
package interpreter

import error.ErrorCompiler
import lexer.{IDENTIFIER, Token}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnvironmentTest extends AnyFunSuite with Matchers {

  test("Environment assign global value, should update global value") {

    val global: Either[ErrorCompiler, Environment] = new Environment().define(Token(IDENTIFIER, "a", 1, Some("a")), 0.0)
    val env: Either[ErrorCompiler, Environment] = global.map(_.createLocalEnv)

    val resultEnv: Option[Environment] = env.toOption.flatMap(_.assign("a", 1.0))

    resultEnv.isDefined shouldBe true
    resultEnv.get.restore.get("a") shouldBe Right(1.0)
  }

  test("Environment assign local value, should local global value") {

    val env: Either[ErrorCompiler, Environment] = new Environment().define(Token(IDENTIFIER, "a", 1, Some("a")), 0.0)

    val resultEnv: Option[Environment] = env.toOption.flatMap(_.assign("a", 1.0))

    resultEnv.isDefined shouldBe true
    resultEnv.get.get("a") shouldBe Right(1.0)
    resultEnv.get.restore.get("a") shouldBe Left("Undefined variable 'a'.")
  }

  test("Environment get have to return the less nested value, should local value") {

    val globalEnv: Either[ErrorCompiler, Environment] = new Environment().define(Token(IDENTIFIER, "a", 1, Some("a")), 0.0)
    val env: Either[ErrorCompiler, Environment] = globalEnv.map(_.createLocalEnv).flatMap(_.define(Token(IDENTIFIER, "a", 1, Some("a")), 1.0))

    val resultEnv: Either[String, Any] = env match {
      case Right(envValue)  => envValue.get("a")
      case Left(err)        => Left(err.message)
    }

    resultEnv shouldBe Right(1.0)
  }

}
